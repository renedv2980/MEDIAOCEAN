*          DATA SET REPRO10    AT LEVEL 033 AS OF 11/02/98                      
*&&      SET   NOP=N                                                            
*PHASE T80A10B                                                                  
T80A10   TITLE 'REPRO10 - PROPROSAL RECORDS - REPORT OVERLAY'                   
***********************************************************************         
* NOTES--                                                                       
*                                                                               
*    PROGRAM IS UNFIT FOR CORERESIDENCEY.  DUE TO PRNTBUFF WHICH                
*      WOULD NEED TO BE MOVED TO THE PROGRAM AREA.                              
***********************************************************************         
PRO10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T80A10*                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
         L     R8,AGWORK           SAVE ADDRESS OF PRINT BUFFER                 
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         LA    R0,PRNTBUFF                                                      
         ST    R0,APRNTBFF                                                      
*                                                                               
         MVC   RPTPARMS,0(R1)      SAVE CALLED PARAMETERS                       
         ST    R1,RPTR1                                                         
*                                                                               
         SRL   RF,24                                                            
         STC   RF,RPTROUT                                                       
*                                                                               
         LA    R0,PRNTBUFF                                                      
         ST    R0,APRNTBFF                                                      
*                                                                               
         GOTO1 VCOLY,BODMCB,(X'1A',0),(0,0)                                     
         CLI   BODMCB+4,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,BODMCB                                                        
         ICM   RF,8,RPTROUT                                                     
         L     R1,RPTR1                                                         
         GOTOX (RF)                                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SAVE PARAMTERS LOCALLY                                                        
***********************************************************************         
         DS    0D                                                               
RPTPARMS DS    XL(6*4)                                                          
RPTR1    DS    F                                                                
RPTROUT  DS    X                                                                
***********************************************************************         
* PRINTLINE(PAGE) BUFFER                                                        
***********************************************************************         
         DS    0D                                                               
PRNTBUFF DS    0X                                                               
         PRINT OFF                                                              
         DC    (BUFFLNS)CL(132)' '                                              
         PRINT ON                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE REPRORWRK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033REPRO10   11/02/98'                                      
         END                                                                    

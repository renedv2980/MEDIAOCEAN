*          DATA SET GEFIL04    AT LEVEL 012 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T00AB4C                                                                  
*INCLUDE GEFILFLT                                                               
*INCLUDE GEFILCOL                                                               
*INCLUDE GEFILACT                                                               
*INCLUDE GEFILREC                                                               
*INCLUDE GEFILACV                                                               
*INCLUDE GEFILDWN                                                               
*INCLUDE GEFILSUB                                                               
*INCLUDE GEFILREQ                                                               
*                                                                               
         TITLE 'GENERALISED ROUTINES OVERLAY'                                   
GFI04    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OWORKL,GEFIL04*,R6,R7,RR=RE                                      
         USING OVERWRKD,RC                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         ST    RE,MYRELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
GSFRR    USING FRRELD,GSFRREL                                                   
PSFRR    USING FRRELD,PSFRREL                                                   
GSFRA    USING FRRELD,GSFRAEL                                                   
PSFRA    USING FRRELD,PSFRAEL                                                   
*                                                                               
         STCM  RF,8,MYTOP          SAVE FOR INIT CALL TO OVERLAY                
         XR    RF,RF                                                            
         ICM   RF,1,GSFRR.FRRPHASE                                              
*                                                                               
         CH    RF,=Y(ROUTSN)                                                    
         BNH   *+8                 UNKNOWN RECORD                               
         B     EXITH                                                            
*                                                                               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RF,ROUTS(RF)                                                     
         A     RF,MYRELO                                                        
         ICM   RF,8,MYTOP          IN CASE OF INIT CALL                         
         GOTO1 (RF),SVPARMS                                                     
         B     EXIT                                                             
*                                                                               
ROUTS    DC    V(FILREC)        1  RECORD HELP                                  
         DC    V(FILACT)        2  ACTION HELP                                  
         DC    V(FILACV)        3  ACTIVITY                                     
         DC    V(FILFLT)        4  FILTER                                       
         DC    V(FILCOL)        5  COLUMN                                       
         DC    V(FILDOWN)       6  DOWNLOAD                                     
         DC    V(FILSUB)        7  SUB-ACTION HELP                              
         DC    V(FILREQ)        8  REQUEST                                      
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 EXIT HIGH                                    
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
MYRELO   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
*                                                                               
MYTOP    DS    XL1                                                              
OWORKL   EQU   *-OVERWRKD                                                       
*        GEFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE GEFILWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012GEFIL04   08/10/11'                                      
         END                                                                    

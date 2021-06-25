*          DATA SET SPNWS97    AT LEVEL 007 AS OF 07/20/93                      
*PHASE T20702A,*                                                                
         TITLE 'T20702A - GENERAL RECORD FIX PROGRAM - SPOTE FIX'               
T20702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20702**,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
*                                                                               
         LA    R2,IOKEY                                                         
         USING BYRRECD,R2                                                       
*                                                                               
         CLI   APMODE,APMVALK        VALIDATE KEY                               
         BE    VALKEY                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         MVI   0(R2),X'0C'                                                      
*                                                                               
FIX0     DS    0H                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FIX1     CLC   IOKEY(1),IOKEYSAV                                                
         BNE   FIXX                                                             
         CLI   IOKEY+1,X'D1'       SATO TV DONE ALREADY                         
         BNE   FIX1A                                                            
         MVI   IOKEY+1,X'D2'                                                    
         XC    IOKEY+2(11),IOKEY+2                                              
         B     FIX0                                                             
*                                                                               
FIX1A    ZIC   RE,IOKEY+1                                                       
         SRL   RE,4                                                             
         CH    RE,=H'4'            AVOID SUTO                                   
         BE    *+12                                                             
         CH    RE,=H'14'           AND SACH                                     
         BNE   FIX1B                                                            
         LA    RE,1(RE)                                                         
         SLL   RE,4                                                             
         STC   RE,IOKEY+1                                                       
         XC    IOKEY+2(11),IOKEY+2                                              
         B     FIX0                                                             
*                                                                               
FIX1B    L     R2,AIOAREA1                                                      
         GOTO1 AIO,FILGETU1                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,24(R2)                                                        
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
FIX2     CLI   0(R1),0                                                          
         BE    FIX6                                                             
         CLI   0(R1),X'15'                                                      
         BNE   FIX4                                                             
         CLI   2(R1),C'A'                                                       
         BNE   *+12                                                             
         MVI   2(R1),C'B'                                                       
         LA    RE,1                                                             
         CLI   2(R1),C'N'                                                       
         BNE   FIX4                                                             
         MVI   2(R1),C'C'                                                       
         LA    RE,1                                                             
*                                                                               
FIX4     IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FIX2                                                             
*                                                                               
FIX6     LTR   RE,RE                                                            
         BZ    FIX8                                                             
         GOTO1 AIO,FILPUT1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FIX8     DS    0H                                                               
         GOTO1 AIO,DIRSQ+IO1                                                    
         BE    FIX1                                                             
         DC    H'0'                                                             
*                                                                               
FIXX     MVC   FVMSGNO,=AL2(FVIMED)                                             
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                                                                  
         DS    X                                                                
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFED                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSDED                                                       
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE SPNWSBYR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPNWS97   07/20/93'                                      
         END                                                                    

*          DATA SET SPNWS97E   AT LEVEL 002 AS OF 07/20/93                      
*PHASE T20702E,*                                                                
         TITLE 'T20702E - GENERAL RECORD FIX PROGRAM - SPOTG FIX'               
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
         ZIC   RE,IOKEY+1                                                       
         SRL   RE,4                                                             
         CH    RE,=H'2'            ONLY INCLUDE WITO                            
         BE    FIX1A                                                            
         CH    RE,=H'5'            AND MKTO                                     
         BE    FIX1A                                                            
         CH    RE,=H'7'            AND SVVA                                     
         BE    FIX1A                                                            
         SLL   RE,4                                                             
         LA    RE,15(RE)                                                        
         STC   RE,IOKEY+1                                                       
         XC    IOKEY+2(11),IOKEY+2                                              
         B     FIX0                                                             
*                                                                               
FIX1A    OC    IOKEY+4(9),IOKEY+4                                               
         BZ    *+14                                                             
         MVC   IOKEY+4(9),=X'FFFFFFFFFFFFFFFFFF'                                
         B     FIX0                                                             
         L     R2,AIOAREA1                                                      
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
**PAN#1  DC    CL21'002SPNWS97E  07/20/93'                                      
         END                                                                    

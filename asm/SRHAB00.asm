*          DATA SET SRHAB00    AT LEVEL 006 AS OF 11/22/91                      
*PHASE T12400A                                                                  
         SPACE 1                                                                
*=========================================================*                     
* THIS CODE IS USED TO OVERWRITE PORTIONS OF DATAMGR WITH *                     
* CODE TO DO A HARD TRACE IN STORAGE.                     *                     
*=========================================================*                     
         TITLE '$LERB/A - DATAMGR HARD TRACE CODE'                              
LERB     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,*$LERB**,RR=RE                                         
         USING WRKD,RC                                                          
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         USING SRPARMD,R1                                                       
         MVC   SRPARS,0(R1)                                                     
         DROP  R1                                                               
         L     RA,SRPAR6           A(TWA)                                       
         USING SRLEBFFD,RA                                                      
         L     R4,SRPAR1           A(SYSFAC)                                    
         USING SYSFACD,R4                                                       
* DESTROY DMADD/REQUEST LOGIC                                                   
         L     RF,VDATAMGR                                                      
         MVC   X'600'(TRACITLN,RF),TRACEIT   <=======                           
         MVC   X'74'(4,RF),=X'47F0B600'      <=======                           
         XIT1                                                                   
*                                                                               
         ORG   LERB+X'600'                   <=======                           
TRACEIT  L     RE,X'DF0'(RA)        X'E30'(RA)=V(SSB) <=====                    
         L     RE,SSBTKADR-SSBD(RE) GET TASK ADDRESS                            
         L     RE,TCBUTL-TCBD(RE)   POINT TO UTL ENTRY                          
         USING UTLD,RE                                                          
*                                                                               
         OC    TSVCREQ,TSVCREQ     TEST FOR SERVICE REQUEST                     
         BNZ   TRACEX              YES - IGNORE                                 
*                                                                               
         L     RF,BUFNEXT                                                       
         CLC   BUFSIN,TSIN         TEST FOR CHANGE OF SIN                       
         BE    *+8                                                              
         L     RF,BUFSTART                                                      
         MVC   BUFSIN,TSIN                                                      
*                                                                               
         XC    0(64,RF),0(RF)                                                   
         L     RE,0(R2)                                                         
         MVC   0(8,RF),0(RE)                                                    
         MVC   7(1,RF),0(R2)         SHOW DMINBTS                               
         L     RE,4(R2)                                                         
         MVC   8(8,RF),0(RE)                                                    
         L     RE,8(R2)                                                         
         ST    RE,16(RF)                                                        
         MVC   16(1,RF),X'6D'(RC)                                               
         MVI   17(RF),0                                                         
         CLC   8(4,RF),TEMP                                                     
         BE    *+10                                                             
         MVC   16(32,RF),0(RE)                                                  
         LA    RF,48(RF)                                                        
         ST    RF,BUFNEXT                                                       
*                                                                               
TRACEX   DC    X'92D5105B'            RESTORE DESTROYED INSTRUCTION             
         B     X'78'(RB)              RETURN TO CALLING INSTRUCTION             
TEMP     DC    C'TEMP'                                                          
*                                                                               
*                                                                               
         DS    0A                                                               
BUFSTART DC    X'002D0000'                                                      
BUFNEXT  DC    F'0'                                                             
BUFSIN   DC    F'0'                                                             
*                                                                               
TRACITLN EQU   *-TRACEIT                                                        
         EJECT                                                                  
WRKD     DSECT                                                                  
SRPARS   DS    0XL24                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
WRKX     EQU   *                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
SRLEBFFD DSECT                                                                  
         DS    CL64                                                             
* SRLEBFFD                                                                      
       ++INCLUDE SRLEBFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRHAB00   11/22/91'                                      
         END                                                                    

*          DATA SET NEOLDMPA0B AT LEVEL 004 AS OF 08/10/00                      
*PHASE T31E0BA                                                                  
         TITLE 'T31E0B - EDIT FOR MPA LIST'                                     
T31E0B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*MPAED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          PASS ARGS TO PRINT IN W/S AREA 2             
         USING MPACOM,R7                                                        
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         MVI   FTERMFLG,0          FIELDS ARE REQUIRED                          
         LA    R2,SPLBOOKH         BOOK                                         
         NETGO NVGETFLD,DMCB       INSURE THAT ITS THERE                        
*                                                                               
         GOTO1 BOOKVAL,DMCB,(C'N',(R2)),(X'01',BOOK),(C'S',SCANNER)             
*                                                                               
         LA    R2,SPLSEQH          SEQUENCE                                     
         MVI   SEQOPT,C'A'         DEFAULT=A                                    
         CLI   5(R2),0                                                          
         BE    ED2                                                              
         MVC   SEQOPT,8(R2)                                                     
         CLI   SEQOPT,C'A'                                                      
         BE    ED2                                                              
         CLI   SEQOPT,C'N'                                                      
         BE    ED2                                                              
         CLI   SEQOPT,C'D'                                                      
         BNE   INVEX                                                            
         SPACE 1                                                                
ED2      LA    R2,SPLBOOKH                                                      
         SPACE 1                                                                
EXIT     XIT1 REGS=(R2)                                                         
         SPACE 1                                                                
INVEX    MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFBD                                                       
*                                                                               
MPACOM   DSECT                                                                  
*** PASSED TO PRINT MODULE                                                      
SEQOPT   DS    CL1                                                              
BOOK     DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEOLDMPA0B08/10/00'                                      
         END                                                                    

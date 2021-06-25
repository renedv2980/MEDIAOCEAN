*          DATA SET PPREPFXOFC AT LEVEL 007 AS OF 01/08/96                      
*PHASE PP0202A,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM TO FIX OFFICE CODES'                    
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*  FIRST, MAKE A TABLE OF BINARY CLIENT CODES AND OFFICE CODES                  
REQF     DS    0H                                                               
         LA    R2,IO                                                            
         LA    R0,TABLEMAX                                                      
         GOTO1 ,BINPARMS,,TABLE,0,L'TABENTRY,(0,3),(R0)                         
         OPEN  (TAPEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
REQF10   GET   TAPEIN,(R2)                                                      
                                                                                
REQF20   MVC   OFC,16(R2)      OFFICE CODE                                      
         MVC   CLNT,0(R2)      OFFICE CODE                                      
         GOTO1 BINSRCH,BINPARMS,(1,TABENTRY)                                    
         OC    BINPARMS,BINPARMS   TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   BINPARMS,X'01'      RECORD NOT FOUND?                            
         BE    REQF10                                                           
         DC    H'0'                SHOULDN'T HAVE ALREADY BEEN THERE            
                                                                                
REQF30   CLOSE TAPEIN                                                           
         LTR   RF,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT5,C'N'                                                       
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      AGENCY,MEDIA                                 
         MVI   KEY+3,X'02'         CLTREC                                       
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLC   KEY(3),QAGENCY     MATCH AGENCY/MEDIA/CLTREC TYPE                
         BNE   EXIT                                                             
         CLI   KEY+3,X'02'                                                      
         BNE   EXIT                                                             
*                                                                               
AGYC6    DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         XC    TABENTRY,TABENTRY                                                
         MVC   CLNT,KEY+4                                                       
         GOTO1 BINSRCH,BINPARMS,(0,TABENTRY)                                    
         CLI   BINPARMS,X'01'      RECORD NOT FOUND?                            
         BNE   AGYC6C              NOT FOUND, SO MUST NOT HAVE BUYS             
         MVC   P(5),=C'ERROR'                                                   
         MVC   P+10(3),CLNT                                                     
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
                                                                                
AGYC6C   DS    0H                                                               
         L     R5,BINPARMS                                                      
         XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,3(R5)                                                   
         CLI   RCWRITE,C'N'                                                     
         BE    AGYC6D                                                           
         GOTO1 PUTPRT                                                           
*                                                                               
AGYC6D   MVC   P(3),PCLTKAGY                                                    
         MVC   P+4(3),PCLTKCLT                                                  
         MVC   P+8(1),PCLTOFF                                                   
         MVC   P+10(2),PCLTAOFC                                                 
         MVC   P+13(4),=C'ACCA'                                                 
         MVC   P+18(2),PCLTACCA                                                 
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=GM,EODAD=REQF30                     
*                                                                               
         DS    0F                                                               
TABENTRY DS    0CL5                                                             
CLNT     DS    CL3                                                              
OFC      DS    CL2                                                              
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
IO       DS    0F                                                               
         DS    CL256                                                            
TABLE    DS    4095XL(L'TABENTRY)  BINARY CLIENT (3), OFFICE CODE (2)           
TABLEMAX EQU   (*-TABLE)/5         MAX ENTRIES                                  
*                                                                               
PP02WRKD DSECT                                                                  
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPFXOFC01/08/96'                                      
         END                                                                    

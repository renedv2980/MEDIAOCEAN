*          DATA SET SPREPFXOFC AT LEVEL 046 AS OF 04/12/99                      
*PHASE SPFX02O                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPFX02 - CONVERT 1 TO 2 CHAR ACC OFFICE CODES'                  
***********************  DO NOT DELETE   ***************************            
* TO CONVERT 1 TO 2 CHAR ACC OFFICE CODES, CHANGE TABLE AND SORT IT!            
***********************  DO NOT DELETE   ***************************            
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
FX       DS    0H                                                               
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
*&&DO                                                                           
         CLC   AGY,=C'BS'                                                       
         BE    FX04                                                             
         CLC   AGY,=C'BT'                                                       
         BNE   FX08                                                             
*                                                                               
FX04     XC    KEY,KEY                                                          
         L     R6,ADAGY                                                         
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING AGYHDRD,R6                                                       
         MVI   AGYOFC2,C'Y'                                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX08                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE',KEY+14,(R6)                   
         DROP  R6                                                               
*&&                                                                             
*                                                                               
FX08     LA    R0,TABLEMAX                                                      
         GOTO1 ,BINPARMS,,TABLE,(R0),L'TABENTRY,(0,3),(R0)                      
         XC    COUNT,COUNT                                                      
         GOTO1 REPORT                                                           
*                                                                               
* NOW, READ THE RECORDS AND MAKE SURE THAT ALL CLIENTS HAVE A NEW CODE          
FX10     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   SVKEY,KEY                                                        
         OI    SVKEY+1,X'04'                                                    
         MVC   SVBGYMD,BAGYMD                                                   
                                                                                
FX20     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(2),SVKEY                                                     
         BH    FXEND                                                            
         OC    KEY+4(9),KEY+4                                                   
         BNZ   FX70                                                             
                                                                                
         CLC   KEY+1(1),SVBGYMD                                                 
         BE    FX23                                                             
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVC   SVBGYMD,KEY+1                                                    
                                                                                
FX23     L     R6,ADCLT                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING CLTHDR,R6                                                        
                                                                                
         GOTO1 CLUNPK,DMCB,KEY+2,P                                              
         MVC   P+15(1),COFFICE                                                  
         MVC   P+30(2),CACCOFC                                                  
         MVC   P+45(2),CACCAGY     ACC AGENCY CODE                              
                                                                                
         XC    TABENTRY,TABENTRY                                                
         MVC   EBAGY,AGY                                                        
         MVC   OFC1,CACCOFC        USE ACC OFFICE                               
         CLI   CACCOFC+1,C' '      IF 2 CHAR ACC OFFICE, SKIP                   
         BH    FX60                                                             
         CLI   CACCOFC,0           IF NO ACC OFFICE CODE,                       
         BNE   *+10                                                             
         MVC   OFC1,COFFICE        USE MEDIA OFFICE CODE                        
                                                                                
FX25     GOTO1 BINSRCH,BINPARMS,(0,TABENTRY)                                    
         CLI   BINPARMS,X'01'      RECORD NOT FOUND?                            
         BNE   FX30                                                             
                                                                                
         MVC   P2(9),=C'NOT FOUND'                                              
         B     FX60                                                             
                                                                                
FX30     DS    0H                                                               
         L     R5,BINPARMS                                                      
         MVC   CACCOFC,3(R5)       NEW OFFICE CODE                              
                                                                                
FX50     CLI   RCWRITE,C'Y'                                                     
         BNE   FX60                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE',KEY+14,(R6)                   
FX60     MVC   P+60(2),CACCOFC     OFFICE CODE                                  
         MVC   P+75(2),CACCAGY     ACC AGENCY CODE                              
         GOTO1 REPORT                                                           
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
                                                                                
FX70     MVC   KEY+4(9),=X'FFFFFFFFFFFFFFFFFF'                                  
         B     FX20                                                             
                                                                                
         DROP  R6                                                               
FXEND    DS    0H                                                               
         MVC   P(5),=C'COUNT'                                                   
         EDIT  COUNT,(10,P+10),0,ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
HDHOOK   NTR1                                                                   
                                                                                
         MVC   H1+55(22),=C'OFFICE CODE CONVERSION'                             
         MVI   H2+55,C'_'                                                       
         MVC   H2+56(21),H2+55                                                  
         B     EXIT                                                             
                                                                                
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,1,C'CLIENT'                                                   
         SSPEC H7,16,C'MEDIA OFFICE'                                            
         SSPEC H7,31,C'OLD ACC OFF'                                             
         SSPEC H7,46,C'OLD AGY'                                                 
         SSPEC H7,61,C'NEW ACC OFF'                                             
         SSPEC H7,76,C'NEW AGY'                                                 
         SSPEC H8,1,C'------'                                                   
         SSPEC H8,16,C'------------'                                            
         SSPEC H8,31,C'-----------'                                             
         SSPEC H8,46,C'-------'                                                 
         SSPEC H8,61,C'-----------'                                             
         SSPEC H8,76,C'-------'                                                 
         DC    X'00'                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
* SEE TABENTRY = EBAGY(2),OFC1(1),OFC2(2)                                       
TABLE    DC    C'YNAAA'                                                         
         DC    C'YNBD3'                                                         
         DC    C'YNCCC'                                                         
         DC    C'YNDBD'                                                         
         DC    C'YNECE'                                                         
         DC    C'YNFCF'                                                         
         DC    C'YNGCG'                                                         
         DC    C'YNHCH'                                                         
         DC    C'YNICI'                                                         
         DC    C'YNJCJ'                                                         
         DC    C'YNKCK'                                                         
         DC    C'YNLCL'                                                         
         DC    C'YNMBM'                                                         
         DC    C'YNNCN'                                                         
         DC    C'YNOCO'                                                         
         DC    C'YNPCP'                                                         
         DC    C'YNQCQ'                                                         
         DC    C'YNRCR'                                                         
         DC    C'YNSCS'                                                         
         DC    C'YNTCT'                                                         
         DC    C'YNUCU'                                                         
         DC    C'YNVCV'                                                         
         DC    C'YNWBW'                                                         
         DC    C'YNXBX'                                                         
         DC    C'YNYCY'                                                         
         DC    C'YNZCZ'                                                         
         DC    C'YN0C0'                                                         
         DC    C'YN1C1'                                                         
         DC    C'YN2C2'                                                         
         DC    C'YN3C3'                                                         
         DC    C'YN4C4'                                                         
         DC    C'YN5C5'                                                         
         DC    C'YN6C6'                                                         
         DC    C'YN7C7'                                                         
         DC    C'YN8C8'                                                         
         DC    C'YN9C9'                                                         
         DC    C'YN#BA'                                                         
         DC    C'YN?BB'                                                         
TABLEMAX EQU   (*-TABLE)/5         MAX ENTRIES                                  
         DS    0F                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
SVKEY    DS    XL2                                                              
CLNT     DS    CL3                                                              
TABENTRY DS    0XL5                                                             
EBAGY    DS    CL2                                                              
OFC1     DS    C                                                                
OFC2     DS    CL2                                                              
SVBGYMD  DS    C                                                                
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
         DC    H'0'                                                             
         DC    H'0'                                                             
IO       DS    0F                                                               
         DS    CL256                                                            
                                                                                
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046SPREPFXOFC04/12/99'                                      
         END                                                                    

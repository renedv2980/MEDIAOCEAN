*          DATA SET CPFILXTRCT AT LEVEL 022 AS OF 05/01/02                      
*PHASE CPFILXTA CPFILXTR                                                        
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'EXTRACT DATA FROM CPP FILE '                                    
COPOUT   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**CPLD**,=A(COPSAVE),R9,R8                                     
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         LA    R5,OUTA                                                          
         USING CPIDREC,R5                                                       
         SPACE 3                                                                
         OPEN  (INTER,(OUTPUT))                                                 
         OPEN  (CPTIN,(INPUT))                                                  
NXTREC   L     R2,=A(IO1)                                                       
         GET   CPTIN,(R2)                                                       
         XC    CRSPOTS,CRSPOTS                                                  
         LA    R2,4(R2)                                                         
         USING CPKEYD,R2                                                        
         CLI   CPKTYPE,X'02'       CLIENT DATA                                  
         BNE   NXTREC                                                           
         LA    R4,CPRECORD                                                      
PROCD    CLI   0(R4),0                                                          
         BE    POSTD                                                            
         CLI   0(R4),2                                                          
         BNE   *+8                                                              
         BAS   R9,XTRDATA                                                       
         CLI   0(R4),86                                                         
         BNE   *+8                                                              
         USING CPPERFD,R4                                                       
         CLI   CPPMONTH,9                                                       
         BNE   *+8                                                              
         BAS   RE,POSTUNPK                                                      
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     PROCD                                                            
         SPACE 2                                                                
POSTD    OC    CRSPOTS,CRSPOTS                                                  
         BZ    NXTREC                                                           
         MVI   CPIDREC,C'D'                                                     
         MVC   CPIDAGY,CPKAGY                                                   
         MVC   CPIDMED,CPKMED                                                   
         MVC   CPIDCLT,CPKCLT                                                   
         MVC   CPIDMKT,CPKMKT                                                   
         MVC   CPIDAFFL,CPKAFFIL                                                
         MVC   CPIDTARG,CPKTARGT                                                
         MVC   CPIDDEMO,CPKDEMO                                                 
         MVC   CPIDSERV,CPKSERV                                                 
         MVC   CPIDDP,CPKDAYPT                                                  
         MVC   CPIDSL,CPKSPTLN                                                  
         MVC   CPIDPROG,CPKPROG                                                 
         MVC   CPIDEQIV,CREQUIV                                                 
         MVI   CPIDYEAR,86                                                      
         MVI   CPIDMON,9                                                        
         MVC   CPIDSPOT,CRSPOTS                                                 
         MVC   CPIDCASH,CRCASH                                                  
         MVC   CPIDPNTS,CRPOINTS                                                
         MVC   CPIDIMPS,CRIMPS                                                  
         MVC   CPIDMKWT,CRMKTWT                                                 
*                                                                               
*        MVC   P(40),CPIDREC                                                    
*        GOTO1 =V(PRINTER)                                                      
*        GOTO1 =V(HEXOUT),DMCB,CPIDREC,P,40                                     
*        GOTO1 =V(PRINTER)                                                      
         PUT   INTER,(R5)                                                       
         L     RE,RECCNT                                                        
         LA    RE,1(RE)                                                         
*        CH    RE,=H'50'                                                        
*        BL    *+6                                                              
*        DC    H'0'                                                             
         ST    RE,RECCNT                                                        
         B     NXTREC                                                           
         SPACE 2                                                                
POSTUNPK NTR1                                                                   
         USING CPPERFD,R4                                                       
         LA    R2,CPPSPOTS         CONVERT ELEMENT TO 4 FULL-WORDS              
         LA    R3,CRSPOTS            SAVE AREA                                  
         LA    R5,4                                                             
         SPACE 2                                                                
POSTUN2  LH    R1,0(R2)                                                         
         XUNPK                                                                  
         ST    R1,0(R3)                                                         
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,POSTUN2                                                       
         B     XIT                                                              
         SPACE 2                                                                
         DROP  R4                                                               
         USING CPDATAD,R4          SEED THE EXTRA DATA                          
XTRDATA  MVC   CREQUIV,CPDEQUIV                                                 
         MVC   CRMKTWT,CPDMKTWT                                                 
         BR    R9                                                               
XIT      XIT1                                                                   
CPTEOF   CLOSE (CPTIN)                                                          
INTEOF   CLOSE (INTER)                                                          
         L     R5,RECCNT                                                        
         MVC   P+10(16),=C'RECORDS EXTRACTED'                                   
         EDIT  RECCNT,(8,P)                                                     
         GOTO1 =V(PRINTER)                                                      
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
RECCNT   DC    F'0'                                                             
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL20                                                             
CREQUIV  DS    CL2                                                              
CRMKTWT  DS    CL2                                                              
CRSPOTS  DS    CL4                                                              
CRCASH   DS    CL4                                                              
CRPOINTS DS    CL4                                                              
CRIMPS   DS    CL4                                                              
OUTA     DS    CL40                                                             
IO1      DS    8400C                                                            
         EJECT                                                                  
INTER    DCB   DDNAME=INTER,           DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00040,                                            X        
               BLKSIZE=04000,          DOS BLKSIZE=04000               X        
               MACRF=PM,                                               X        
               EODAD=INTEOF                                                     
         SPACE 2                                                                
CPTIN    DCB   DDNAME=CPTIN,           DOS SYS006                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=08196,                                            X        
               BLKSIZE=08200,          DOS BLKSIZE=08200               X        
               MACRF=GM,                                               X        
               EODAD=CPTEOF                                                     
COPSAVE  DS    20000C                                                           
         EJECT                                                                  
       ++INCLUDE CPGENINTER                                                     
       ++INCLUDE CPGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022CPFILXTRCT05/01/02'                                      
         END                                                                    

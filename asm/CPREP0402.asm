*          DATA SET CPREP0402  AT LEVEL 045 AS OF 05/01/02                      
*PHASE CP0402A                                                                  
         TITLE 'SUPERTARGET COMPOSITION REPORT'                                 
CP0402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CP04**,RR=R2                                                 
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING CPWORKD,RA,R9                                                    
         ST    R2,RELO                                                          
         MVI   FCMONTH,C'Y'                                                     
         CLI   MODE,REQFRST                                                     
         BNE   DL1                                                              
         MVC   ALLCASH,=PL8'0'                                                  
         MVC   ALLSPOT,=PL8'0'                                                  
         LA    RE,SUMBUFA                                                       
         ST    RE,ANEXT                                                         
         L     RF,=F'20000'                                                     
         XCEF                                                                   
         MVI   FCMONTH,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
DL1      CLI   MODE,REQLAST                                                     
         BNE   DL2                                                              
         BAS   RE,PRS                                                           
         MVC   P1+20(6),=C'SPOTS='                                              
         EDIT  (P8,ALLCASH),(15,P+1),COMMAS=YES                                 
         EDIT  (P8,ALLSPOT),(15,P+26),COMMAS=YES                                
         GOTO1 REPORT                                                           
         SPACE 2                                                                
DL2      CLI   MODE,CLTFRST                                                     
         BNE   DL4                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR DATA RECORDS                                        
         SPACE 3                                                                
DL4      CLI   MODE,PROCDATA                                                    
         BNE   XIT                                                              
         L     R2,ADDATA                                                        
         USING CPKEYD,R2                                                        
         ZIC   R3,CPKTARGT                                                      
         LTR   R3,R3                                                            
         BZ    DL5                                                              
         BCTR  R3,0                                                             
         MH    R3,=H'7'                                                         
         L     R1,ADDEMBUF                                                      
         AR    R3,R1                                                            
         MVC   CURTGT,0(R3)                                                     
         ZIC   R3,CPKDEMO                                                       
         BCTR  R3,0                                                             
         MH    R3,=H'7'                                                         
         L     R1,ADDEMBUF                                                      
         AR    R3,R1                                                            
         MVC   CURDEM,0(R3)                                                     
DL5      L     R3,=A(DPLUTAB)                                                   
         A     R3,RELO                                                          
         SPACE 2                                                                
DL10     DS    0H                                                               
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
DL12     CLI   0(R4),0                                                          
         BE    DLX                                                              
         LA    R8,SUMBUFA                                                       
         USING SUMBUFD,R8                                                       
         CLI   SUMBTGT,0                                                        
         BE    DL14                                                             
         CLC   CURTGT,SUMBTGT                                                   
         BE    DL14                                                             
         BAS   RE,PRS                                                           
         MVI   P,X'00'                                                          
         GOTO1 REPORT                                                           
         ST    R8,ANEXT                                                         
         SPACE 2                                                                
DL14     L     R8,ANEXT                                                         
         MVC   SUMBTGT,CURTGT                                                   
         MVC   SUMBDEM,CURDEM                                                   
         CLI   0(R4),2                                                          
         BNE   DL18                                                             
         USING CPDATAD,R4                                                       
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         L     R3,NPERIODS                                                      
         SPACE 2                                                                
DL16     OC    CPSPOTS(16),CPSPOTS                                              
         BZ    DL17A                                                            
         L     R1,CPCASH           OVERALL TOTALS                               
         CVD   R1,DUB                                                           
         AP    ALLCASH,DUB                                                      
         L     R1,CPSPOTS          OVERALL TOTALS                               
         CVD   R1,DUB                                                           
         AP    ALLSPOT,DUB                                                      
         L     R1,CPSPOTS                                                       
         A     R1,SUMBSPT                                                       
         ST    R1,SUMBSPT                                                       
         L     R1,CPCASH                                                        
         A     R1,SUMBDOL                                                       
         ST    R1,SUMBDOL                                                       
         L     R1,CPOINTS                                                       
         A     R1,SUMBRTG                                                       
         ST    R1,SUMBRTG                                                       
         L     R1,CPIMPS                                                        
         A     R1,SUMBIMP                                                       
         ST    R1,SUMBIMP                                                       
*                                                                               
         L     R1,CPSPOTS                                                       
         A     R1,TOTSPT                                                        
         ST    R1,TOTSPT                                                        
         L     R1,CPCASH                                                        
         A     R1,TOTDOL                                                        
         ST    R1,TOTDOL                                                        
         L     R1,CPOINTS                                                       
         A     R1,TOTRTG                                                        
         ST    R1,TOTRTG                                                        
         L     R1,CPIMPS                                                        
         A     R1,TOTIMP                                                        
         ST    R1,TOTIMP                                                        
         SPACE 2                                                                
DL17     DS    0H                                                               
DL17A    A     R4,WIDTHPER                                                      
         BCT   R3,DL16                                                          
         SPACE 2                                                                
DL18     ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     DL12                                                             
*                                                                               
DLX      LA    R8,L'SUMBUF(R8)                                                  
         ST    R8,ANEXT                                                         
         B     XIT                                                              
         EJECT                                                                  
PRS      NTR1                                                                   
         LA    R8,SUMBUFA                                                       
         USING SUMBUFD,R8                                                       
         LA    R7,P1                                                            
         USING SPRD,R7                                                          
PRS2     OC    SUMBTGT,SUMBTGT                                                  
         BZ    PRSX                                                             
         MVC   SPRSUP,SUMBTGT                                                   
         MVC   SPRACT,SUMBDEM                                                   
         EDIT  SUMBSPT,(9,SPRSPT),,COMMAS=YES                                   
         L     R5,SUMBSPT                                                       
         L     R1,TOTSPT                                                        
         BAS   RE,GETPCT                                                        
         EDIT  (R5),(3,SPRPCTS)                                                 
         EDIT  SUMBDOL,(12,SPRDOL),,COMMAS=YES                                  
         L     R5,SUMBDOL                                                       
         L     R1,TOTDOL                                                        
         BAS   RE,GETPCT                                                        
         EDIT  (R5),(3,SPRPCTD)                                                 
         EDIT  SUMBRTG,(11,SPRRTG),1,COMMAS=YES                                 
         L     R5,SUMBRTG                                                       
         L     R1,TOTRTG                                                        
         BAS   RE,GETPCT                                                        
         EDIT  (R5),(3,SPRPCTR)                                                 
         EDIT  SUMBIMP,(11,SPRIMP),,COMMAS=YES                                  
         L     R5,SUMBIMP                                                       
         L     R1,TOTIMP                                                        
         BAS   RE,GETPCT                                                        
         EDIT  (R5),(3,SPRPCTI)                                                 
         GOTO1 REPORT                                                           
         LA    R8,L'SUMBUF(R8)                                                  
         B     PRS2                                                             
*                                                                               
PRSX     XC    TOTACC,TOTACC                                                    
         LA    RE,SUMBUFA                                                       
         LA    RF,4000                                                          
         XCEF                                                                   
         B     XIT                                                              
*                                                                               
GETPCT   LTR   R5,R5                                                            
         BZR   RE                                                               
         SR    R4,R4                                                            
         M     R4,=F'1000'                                                      
         DR    R4,R1                                                            
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
         BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
ALLCASH  DC    PL8'0'                                                           
ALLSPOT  DC    PL8'0'                                                           
RELO     DS    A                                                                
CURTGT   DS    CL7                                                              
CURDEM   DS    CL7                                                              
ANEXT    DS    F                                                                
TOTACC   DS    0CL16                                                            
TOTSPT   DS    F                                                                
TOTDOL   DS    F                                                                
TOTRTG   DS    F                                                                
TOTIMP   DS    F                                                                
         SPACE 2                                                                
SUMBUFA  DS    20000C                                                           
*        INCLUDE CPGENDPLUT                                                     
         PRINT OFF                                                              
       ++INCLUDE CPGENDPLUT                                                     
         PRINT ON                                                               
SUMBUFD  DSECT                                                                  
SUMBUF   DS    0CL36                                                            
SUMBTGT  DS    CL7                                                              
SUMBDEM  DS    CL7                                                              
         DS    CL2                                                              
SUMBSPT  DS    CL4                                                              
SUMBDOL  DS    CL4                                                              
SUMBRTG  DS    CL4                                                              
SUMBIMP  DS    CL4                                                              
         SPACE 2                                                                
SPRD     DSECT                                                                  
SPRSUP   DS    CL7                                                              
         DS    CL2                                                              
SPRACT   DS    CL7                                                              
SPRSPT   DS    CL9                                                              
         DS    C                                                                
SPRPCTS  DS    CL3                                                              
SPRDOL   DS    CL12                                                             
         DS    C                                                                
SPRPCTD  DS    CL3                                                              
         DS    C                                                                
SPRRTG   DS    CL11                                                             
         DS    C                                                                
SPRPCTR  DS    CL3                                                              
         DS    C                                                                
SPRIMP   DS    CL11                                                             
         DS    C                                                                
SPRPCTI  DS    CL3                                                              
         SPACE 2                                                                
*        INCLUDE CPREPWORKD                                                     
*        INCLUDE CPREPMODES                                                     
*        INCLUDE CPGENFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045CPREP0402 05/01/02'                                      
         END                                                                    

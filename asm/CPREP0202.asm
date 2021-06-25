*          DATA SET CPREP0202  AT LEVEL 013 AS OF 09/01/00                      
*PHASE CP0202A                                                                  
         TITLE 'CPP DATA LIST PROGRAM'                                          
CP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CP02**,RR=R2                                                 
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING CPWORKD,RA,R9                                                    
         ST    R2,RELO                                                          
         MVI   FCMONTH,C'Y'                                                     
         CLI   MODE,REQFRST                                                     
         BNE   DL2                                                              
         MVI   FCMONTH,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
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
         MVC   P(24),MKTNAME                                                    
         EDIT  (2,MARKET),(3,P+25)                                              
         EDIT  (2,MKTRANK),(3,P+29)                                             
         ZIC   R3,CPKTARGT                                                      
         LTR   R3,R3                                                            
         BZ    DL5                                                              
         BCTR  R3,0                                                             
         MH    R3,=H'7'                                                         
         L     R1,ADDEMBUF                                                      
         AR    R3,R1                                                            
         MVC   P+39(7),0(R3)                                                    
         ZIC   R3,CPKDEMO                                                       
         BCTR  R3,0                                                             
         MH    R3,=H'7'                                                         
         L     R1,ADDEMBUF                                                      
         AR    R3,R1                                                            
         MVC   P+47(7),0(R3)                                                    
DL5      L     R3,=A(DPLUTAB)                                                   
         A     R3,RELO                                                          
         SPACE 2                                                                
         MVC   P+55(1),CPKDAYPT                                                 
         MVC   P+57(1),CPKSERV                                                  
         SPACE 2                                                                
DL10     EDIT  (1,CPKSPTLN),(3,P+60)                                            
         MVC   P+64(1),CPKPROG                                                  
         MVC   P+66(1),CPKAFFIL                                                 
         LR    R4,R2                                                            
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 2                                                                
DL12     CLI   0(R4),0                                                          
         BNE   DL14                                                             
         GOTO1 REPORT                                                           
         B     XIT                                                              
         SPACE 2                                                                
DL14     CLI   0(R4),2                                                          
         BNE   DL18                                                             
         USING CPDATAD,R4                                                       
         EDIT  (2,CPDEQUIV),(4,P+68)                                            
         EDIT  (2,CPDMKTWT),(6,DMCB),3                                          
         MVC   P+32(5),DMCB                                                     
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         L     R3,NPERIODS                                                      
         SPACE 2                                                                
DL16     OC    CPSPOTS(16),CPSPOTS                                              
         BZ    DL17A                                                            
         MVC   P+73(5),CPSTART                                                  
         L     R1,CPSPOTS                                                       
         EDIT  (R1),(6,P+78)                                                    
         L     R1,CPCASH                                                        
         LR    R7,R1                                                            
         EDIT  (R1),(7,P+84),MINUS=YES                                          
         L     R1,CPOINTS                                                       
         EDIT  (R1),(6,P+91),1                                                  
         LR    R5,R1                                                            
         L     R1,CPIMPS                                                        
         EDIT  (R1),(6,P+97)                                                    
         MVC   P+105(4),=C'HIGH'                                                
         M     R6,=F'200'                                                       
         LTR   R5,R5                                                            
         BZ    DL17                                                             
         DR    R6,R5                                                            
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         CH    R7,=H'9999'                                                      
         BH    DL17                                                             
         EDIT  (R7),(5,P+104),1                                                 
         SPACE 2                                                                
DL17     GOTO1 REPORT                                                           
DL17A    A     R4,WIDTHPER                                                      
         BCT   R3,DL16                                                          
         SPACE 2                                                                
DL18     ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     DL12                                                             
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELO     DS    A                                                                
         PRINT OFF                                                              
       ++INCLUDE CPGENDPLUT                                                     
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013CPREP0202 09/01/00'                                      
         END                                                                    

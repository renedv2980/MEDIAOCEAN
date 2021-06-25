*          DATA SET ACREP7902  AT LEVEL 007 AS OF 03/08/07                      
*PHASE AC7902A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'ANALYSIS CODE LISTING'                                          
AC7902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC79,RR=R3                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PROGD,RC                                                         
         ST    R3,PRELOC                                                        
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   ACL2                                                             
         L     RF,=A(SORTAREA)                                                  
         A     RF,PRELOC                                                        
         ST    RF,ASORTAR                                                       
         LA    RF,P+1                                                           
         ST    RF,POINTER                                                       
         MVI   SW,1                                                             
         MVC   PAGE(2),=H'1'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVOLD,SPACES                                                     
         CLI   QOPT1,C'Y'          SORTING OPTION                               
         BNE   XIT                                                              
         BAS   RE,SETSORT                                                       
         B     XIT                                                              
         EJECT                                                                  
ACL2     CLI   MODE,PROCWORK                                                    
         BNE   ACL100                                                           
         BAS   RE,BUILDREC                                                      
         CLI   QOPT1,C'Y'                                                       
         BE    ACL4                                                             
         BAS   RE,PRINTREC         PRINT IF STRAIGHT                            
         B     XIT                                                              
*                                                                               
ACL4     BAS   RE,PUTSORT          OR POP AWAY IF SORTING                       
         B     XIT                                                              
         EJECT                                                                  
ACL100   CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         CLI   QOPT1,C'Y'          IF SORTING JUST PRINT LAST                   
         BNE   ACL110                                                           
         LA    R6,IOA              OTHERWISE READ SORTED RECORDS                
ACL102   BAS   RE,GETSORT                                                       
         CLC   IOA(L'SORTKEY),SPACES                                            
         BE    ACL110              END OF FILE                                  
         BAS   RE,PRINTREC                                                      
         B     ACL102                                                           
*                                                                               
ACL110   CLC   P,SPACES                                                         
         BE    XIT                                                              
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              BUILD A STANDARD RECORD FOR PRINT OR SORT                        
         SPACE 2                                                                
BUILDREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         LR    RE,R6                                                            
         LA    RF,L'SORTREC                                                     
         XCEF                                                                   
         L     R3,ADACC                                                         
         SR    RF,RF                                                            
         AH    R3,DATADISP                                                      
BLD2     CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),X'12'                                                      
         BE    BLD4                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     BLD2                                                             
         USING ACANALD,R3                                                       
BLD4     MVC   SORTNAME,ACANDESC   NAME TO KEY                                  
         L     R3,ADACC                                                         
         MVC   SORTDATA,0(R3)                                                   
         MVC   SORTUL,2(R3)        AND U/L TO KEY                               
         B     XIT                                                              
         EJECT                                                                  
*              PRINT A STANDARD RECORD                                          
         SPACE 2                                                                
PRINTREC NTR1                                                                   
         LA    R6,IOA                                                           
         USING SORTRECD,R6                                                      
         LA    R3,SORTDATA                                                      
         USING ACANALD,R3                                                       
         AH    R3,DATADISP                                                      
         SPACE 2                                                                
PRT12    CLI   0(R3),X'00'                                                      
         BE    XIT                                                              
         CLI   0(R3),X'12'                                                      
         BE    PRT16                                                            
         SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     PRT12                                                            
PRT16    LA    R5,SORTDATA                                                      
         CLC   QCOMPANY,1(R5)                                                   
         BNE   XIT                                                              
         CLC   2(2,R5),SVOLD       LEDGER CHANGE                                
         BE    PRT17                                                            
         CLC   P,SPACES            SEE IF ANYTHING TO PRINT                     
         BE    *+8                 FROM LAST LEDGER                             
         BAS   RE,MYREPORT                                                      
         MVI   FORCEHED,C'Y'       RESET POINTER                                
         LA    R2,P+1                                                           
         ST    R2,POINTER                                                       
         MVI   SW,1                                                             
         MVC   SVOLD,2(R5)                                                      
PRT17    CLI   QLEDGER,C' '                                                     
         BE    PRT18                                                            
         CLC   QLEDGER,3(R5)                                                    
         BNE   XIT                                                              
PRT18    CLI   QUNIT,C' '                                                       
         BE    PRT20                                                            
         CLC   QUNIT,2(R5)                                                      
         BNE   XIT                                                              
PRT20    L     R2,POINTER                                                       
         MVC   0(2,R2),ACANCODE                                                 
         MVC   11(15,R2),ACANDESC                                               
         LA    R2,40(R2)                                                        
         ZIC   R3,SW                                                            
         LA    R3,1(R3)                                                         
         CH    R3,=H'4'                                                         
         BNE   PRT22                                                            
         LA    R3,1                                                             
         LA    R2,P+1                                                           
         MVI   SPACING,2                                                        
         BAS   RE,MYREPORT                                                      
PRT22    ST    R2,POINTER                                                       
         STC   R3,SW                                                            
         B     XIT                                                              
         EJECT                                                                  
MYREPORT NTR1                                                                   
         MVC   HEAD4+12(1),SVOLD   UNIT                                         
         MVC   HEAD5+12(1),SVOLD+1 LEDGER                                       
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              SORTER INTERFACE                                                 
         SPACE 2                                                                
         USING SORTRECD,R6                                                      
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,(40,ASORTAR),RR=PRELOC         
         B     XIT                                                              
         SPACE 2                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOA,RR=PRELOC                            
         B     XIT                                                              
         SPACE 2                                                                
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=PRELOC                              
         MVC   IOA(L'SORTKEY),SPACES                                            
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVC   0(250,R6),0(R1)                                                  
         B     XIT                                                              
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,17,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=250'                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PROGD    DSECT                                                                  
PRELOC   DS    F                                                                
ASORTAR  DS    A                                                                
POINTER  DS    F                                                                
SW       DS    CL1                                                              
SVOLD    DS    CL2                                                              
IOA      DS    250C                                                             
         SPACE 2                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL250                                                           
SORTKEY  DS    0CL17                                                            
SORTUL   DS    CL2                                                              
SORTNAME DS    CL15                                                             
SORTDATA DS    CL233                                                            
         EJECT                                                                  
* ACGENMODES                                                                    
* ACREPWORKD                                                                    
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
SORTAREA CSECT                                                                  
         DS    40960C              40K FOR SORT                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREP7902 03/08/07'                                      
         END                                                                    

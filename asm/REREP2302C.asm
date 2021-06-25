*          DATA SET REREP2302C AT LEVEL 063 AS OF 05/01/02                      
*PHASE RE2302C,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIND BAD INV HEADER RECORDS'                          
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2302 (RE2302) --- REP FILE MARKER                   *             
*                                                                 *             
* SCHT - MODULE IS TO FIND MISMATCHED STATUS IN                   *             
*        BOTH DIRECTORY AND FILE RECORDS FOR INV (X'12')          *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2302,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
*                                                                               
         LA    R6,KEY                                                           
         MVI   KEY,X'12'                                                        
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
         USING REINVREC,R6                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IOAREA                                                        
*                                                                               
         CLC   0(27,R6),0(R4)      KEYS MATCH?                                  
         BNE   PCKEYERR                                                         
*                                                                               
         CLC   27(1,R6),29(R4)     SAME STATUS?                                 
         BNE   PCSTAERR                                                         
*                                                                               
         B     PCSEQ               GOOD INV RECORD, GO TO NEXT ONE              
*                                                                               
PCKEYERR DS    0H                  KEYS DON'T MATCH                             
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA                                                  
         MVC   P+10(4),RINVKINV                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+16,3,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+24)                                
*                                                                               
         GOTO1 HEXOUT,DMCB,RINVKSRC,P+33,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RINVKBK,P+36,2,=C'TOG'                               
*                                                                               
         MVC   P+50(19),=C'<=== KEYS NOT EQUAL'                                 
         B     PRNTERR                                                          
*                                                                               
PCSTAERR DS    0H                                                               
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA                                                  
         MVC   P+10(4),RINVKINV                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,RINVKSTD,P+16,3,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+24)                                
*                                                                               
         GOTO1 HEXOUT,DMCB,RINVKSRC,P+33,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RINVKBK,P+36,2,=C'TOG'                               
*                                                                               
         CLI   29(R4),X'80'        IS FILE MARKED DELETED?                      
         BNE   *+10                                                             
         MVC   P+50(17),=C'<=== FILE DELETED'                                   
         B     PRNTERR                                                          
*                                                                               
PRNTERR  DS    0H                                                               
         LA    R4,IOAREA           PRINT ACTIVITY DATE                          
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING RINVAEL,R4                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RINVALST),(5,P+41)                                
         DROP  R4                                                               
*                                                                               
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         GOTO1 REPORT                                                           
         B     PCSEQ                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT ======>'                                          
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R4,34,ELCODE                                                     
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
*                                                                               
COUNT    DS    F                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2302,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
MYKEY    DS    CL32                                                             
ELCODE   DS    XL1                                                              
*                                                                               
IOAREA   DS    XL4000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063REREP2302C05/01/02'                                      
         END                                                                    

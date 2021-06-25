*          DATA SET REREP2402T AT LEVEL 032 AS OF 04/30/99                      
*PHASE RE2402T,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO CHECK BAD RECORDS ON FILE'                            
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2402,R9,RR=R5                                              
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
         XC    SVKEY,SVKEY                                                      
         XC    COUNT,COUNT                                                      
         XC    MYFLAG,MYFLAG                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    PC20                                                             
         DC    H'0'                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PC20     DS    0H                                                               
         LA    R6,KEY                                                           
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
*                                                                               
PC50     GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IOAREA,DMWORK             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IOAREA                                                        
*                                                                               
         CLC   RINVKEY,0(R4)                                                    
         BE    PCSEQ                                                            
*                                                                               
         MVC   P(2),RINVKREP                                                    
         MVC   P+3(5),RINVKSTA                                                  
         MVC   P+10(4),RINVKINV                                                 
*                                                                               
*        GOTO1 HEXOUT,DMCB,RINVKSTD,P+16,3,=C'TOG'                              
*        GOTO1 HEXOUT,DMCB,RINVKSRC,P+27,1,=C'TOG'                              
*        GOTO1 HEXOUT,DMCB,RINVKBK,P+31,2,=C'TOG'                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+16)                                
*                                                                               
         MVC   P+27(2),=C'N,'                                                   
*                                                                               
         XC    TMPDATE,TMPDATE                                                  
         MVC   TMPDATE(1),RINVKBK  YEAR                                         
         MVC   TMPDATE+1(1),RINVKBK+1    MONTH                                  
         MVI   TMPDATE+2,X'01'     DAY                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,TMPDATE),(5,EBCDATE)                              
         MVC   P+29(3),EBCDATE                                                  
         MVC   P+32(2),EBCDATE+6                                                
         MVC   P+34(3),=C'(H)'                                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
*        GOTO1 HEXOUT,DMCB,RINVKEY,P,27,=C'TOG'                                 
*        GOTO1 HEXOUT,DMCB,KEY+27,P+60,4,=C'TOG'                                
*        GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
PC150    L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
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
MYFLAG   DS    XL1                 FLAGS                                        
DELREC   EQU   X'01'               DELETE THIS RECORD                           
*                                                                               
COUNT    DS    F                                                                
SVKEY    DS    CL27                                                             
*                                                                               
PURGEKEY DS    CL24                                                             
*                                                                               
TMPDATE  DS    XL3                 INV END DATE (BINARY)                        
EBCDATE  DS    CL8                                                              
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'BL'                                                            
         DC    X'0000'                                                          
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
IOAREA   DS    XL2000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
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
**PAN#1  DC    CL21'032REREP2402T04/30/99'                                      
         END                                                                    

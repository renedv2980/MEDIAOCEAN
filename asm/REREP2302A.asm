*          DATA SET REREP2302A AT LEVEL 058 AS OF 03/20/00                      
*PHASE RE2302A,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO CHECK BAD INV RECORDS ON FILE'                        
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2302 (RE2302) --- REP FILE MARKER                   *             
*                                                                 *             
*        THIS MODULE CHECKS THE FOLLOWING:                        *             
*              1. ANY EFF. DATE ENTERED > 12/31/26                *             
*              2. ANY EFF. END DATE > EFF. START DATE             *             
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
         XC    SAVEKEY,SAVEKEY                                                  
         XC    COUNT,COUNT                                                      
         XC    CHKCOUNT,CHKCOUNT                                                
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
         L     RF,CHKCOUNT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,CHKCOUNT                                                      
*                                                                               
PC20     DS    0H                                                               
         LA    R6,KEY                                                           
*                                                                               
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
         CLI   RINVKSRC,0          ONLY HEADERS                                 
         BNE   PCSEQ                                                            
*                                                                               
PC25     DS    0H                                                               
         MVI   MYFLAG,0                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
*                                                                               
         CLI   RINVPEFF+2,0        ANY END DATE                                 
         BE    PC50                                                             
*                                                                               
         CLC   RINVPEFF(2),RINVPEFF+2                                           
         BNH   PC50                                                             
         OI    MYFLAG,EFFOVLP                                                   
         B     PC100                                                            
*                                                                               
PC50     DS    0H                                                               
         TM    RINVPEFF,X'80'      IS HIGH ORDER BIT ON                         
         BZ    PC100                                                            
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    *+12                                                             
         TM    RINVPEFF+2,X'80'    IS HIGH ORDER BIT ON                         
         BZ    PC100                                                            
*                                                                               
         CLC   RINVPEFF(2),=XL2'FD9F' START DATE > 12/31/26                     
         BH    PC100                                                            
         CLC   RINVPEFF(2),=XL2'A021' START DATE < 1/1/80                       
         BL    PC100                                                            
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    PCSEQ                                                            
*                                                                               
         CLC   RINVPEFF+2(2),=XL2'FD9F' END DATE >12/31/26                      
         BH    PC100                                                            
         CLC   RINVPEFF+2(2),=XL2'A021' END DATE < 1/1/80                       
         BL    PC100                                                            
         B     PCSEQ                                                            
*                                                                               
PC100    DS    0H                                                               
         MVC   P(2),RINVKREP                                                    
         MVC   P+4(5),RINVKSTA                                                  
         MVC   P+11(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+17)                                
*                                                                               
         TM    MYFLAG,EFFOVLP                                                   
         BZ    *+10                                                             
         MVC   P+30(20),=C'STRT DATE > END DATE'                                
*                                                                               
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
         DC    CL4'DATA'                                                        
COUNT    DS    F                                                                
CHKCOUNT DS    F                                                                
SAVEKEY  DS    CL27                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2302,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
MYFLAG   DS    XL1                                                              
EFFOVLP  EQU   X'01'               START DATE > END DATE                        
*                                                                               
IO       DS    XL2000                                                           
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
**PAN#1  DC    CL21'058REREP2302A03/20/00'                                      
         END                                                                    

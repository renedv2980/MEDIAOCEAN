*          DATA SET REREP2402B AT LEVEL 221 AS OF 01/12/00                      
*          DATA SET REREP2402A AT LEVEL 243 AS OF 09/09/99                      
*PHASE RE2402B,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIND CONTRACTS WITH BAD HEX 11 ELEMENTS'              
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 10JAN00 (MLB) DATE OF CONCEPTION                                *             
*                                                                 *             
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
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0               FOUND RECORD COUNTER                         
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVI   RCONKTYP,X'0C'                                                   
         DROP  R6                                                               
*                                                                               
         DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    CT20                                                             
         DC    H'0'                                                             
*                                                                               
CTSEQ    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CT20     DS    0H                                                               
         CLI   KEY,X'0C'                                                        
         BNE   PCX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'11'        GETTING COMMENTS ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   CTSEQ                                                            
*                                                                               
         USING RCONBCEL,R6                                                      
CT30     CLI   RCONBCLN,62                                                      
         BNH   CT40                                                             
*                                                                               
         MVC   P(2),KEY+2                                                       
         GOTO1 =V(HEXOUT),DMCB,KEY+23,P+10,4                                    
         AHI   R0,1                                                             
         GOTO1 REPORT                                                           
*                                                                               
CT40     BAS   RE,NEXTEL                                                        
         BNE   CTSEQ                                                            
         B     CT30                                                             
         DROP  R6                                                               
*                                                                               
*******************                                                             
*                                                                               
PCX      DS    0H                                                               
         MVC   P(25),=C'NUMBER OF RECORDS FOUND: '                              
*        CHI   R0,0                                                             
*        BNE   *+10                                                             
         EDIT  (R0),(7,P+27)                                                    
*        EDIT  (P5,COUNT),(7,P+11)                                              
         GOTO1 REPORT                                                           
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
COUNT    DC    PL5'0'                                                           
KCOUNT   DC    PL5'0'                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2402,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
REPLIST  DC    C'BL'                                                            
         DC    X'0000'                                                          
*EPLIST  DC    C'CMD4GPIFI1I2I8I9MGRMS1TO'                                      
*                                                                               
         LTORG                                                                  
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
CONKEY   DS    CL(L'KEY)                                                        
DARKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
SVCONNUM DS    CL4                                                              
*                                                                               
IOAREA   DS    CL4000                                                           
CONAREA  DS    CL4000                                                           
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
*  ++INCLUDE REGENDAR                                                           
*  ++INCLUDE REGENCON                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'221REREP2402B01/12/00'                                      
         END                                                                    

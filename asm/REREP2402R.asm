*          DATA SET REREP2402R AT LEVEL 255 AS OF 04/10/97                      
*PHASE RE2402R,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO FIX REP TO SPOT TRANSER BUG'                          
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 03JAN96 (SKU) DATE OF CONCEPTION                                *             
*                                                                 *             
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                *           
*                                                                 *             
* TEMP VERSION WILL BE DELETED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *             
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
         LA    R6,KEY                                                           
         USING RMKGREC,R6                                                       
         XC    KEY,KEY             READ ALL DARE RECORDS                        
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,=C'PV'                                                  
         DROP  R6                                                               
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
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLI   KEY,X'11'                                                        
         BNE   PCX                                                              
         CLC   =C'PV',RMKGKREP                                                  
         BNE   PCX                                                              
         CLC   =C'KTTV',RMKGKSTA                                                
         BE    PC22                                                             
         CLC   =C'WFLD',RMKGKSTA                                                
         BE    PC22                                                             
         CLC   =C'WTXF',RMKGKSTA                                                
         BE    PC22                                                             
         CLC   =C'WFXT',RMKGKSTA                                                
         BE    PC22                                                             
         CLC   =C'WNYW',RMKGKSTA                                                
         BNE   PCSEQ                                                            
*                                                                               
PC22     DS    0H                                                               
         OC    RMKGKPLN(6),RMKGKPLN                                             
         BNZ   PCSEQ                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MKGKEY,KEY                                                       
         MVC   RMKGKREP,=C'FN'                                                  
         DROP  R6                                                               
*                                                                               
*        LA    R6,IOAREA                                                        
*        USING RMKGREC,R6                                                       
*        TM    RMKGSCST,X'80'                                                   
*        BO    PC30                                                             
*        DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PC30                                                             
*                                                                               
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),MKGKEY+15(4)                                          
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),WORK+15(5)                                            
         MVO   WORK+5(5),WORK+10(5)                                             
         PACK  WORK(1),WORK+8(1)                                                
         PACK  WORK+1(1),WORK+7(1)                                              
         PACK  WORK+2(1),WORK+6(1)                                              
         PACK  WORK+3(1),WORK+5(1)                                              
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,WORK,P,4                                         
         MVC   P+12(2),MKGKEY+19                                                
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         TM    RMKGSCST,X'80'                                                   
         BZ    PC23                                                             
         MVC   P+16(8),=C'APPROVED'                                             
         B     PC29                                                             
PC23     DS    0H                                                               
         TM    RMKGSCST,X'20'                                                   
         BZ    PC24                                                             
         MVC   P+16(8),=C'RECALLED'                                             
         B     PC29                                                             
PC24     DS    0H                                                               
         TM    RMKGSCST,X'10'                                                   
         BZ    PC25                                                             
         MVC   P+16(8),=C'REJECTED'                                             
         B     PC29                                                             
PC25     DS    0H                                                               
         TM    RMKGSCST,X'08'                                                   
         BZ    PC26                                                             
         MVC   P+16(9),=C'CANCELLED'                                            
         B     PC29                                                             
PC26     DS    0H                                                               
         TM    RMKGSCST,X'04'                                                   
         BZ    PC27                                                             
         MVC   P+16(7),=C'REVISED'                                              
         B     PC29                                                             
PC27     DS    0H                                                               
         MVC   P+16(3),=C'NEW'                                                  
PC29     DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
PC30     DS    0H                                                               
         MVC   KEY(27),MKGKEY                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    PCSEQ                                                            
         DC    H'0'                                                             
*                                                                               
PCX      DS    0H                                                               
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
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
ELCODE   DS    X                                                                
SYSCODE  DS    CL2                                                              
MKGKEY   DS    CL(L'KEY)                                                        
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
SVCONNUM DS    CL4                                                              
*                                                                               
IOAREA   DS    CL2000                                                           
CONAREA  DS    CL2000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
*      ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'255REREP2402R04/10/97'                                      
         END                                                                    

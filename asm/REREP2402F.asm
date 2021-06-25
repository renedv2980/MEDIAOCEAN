*          DATA SET REREP2402F AT LEVEL 252 AS OF 06/11/98                      
*PHASE RE2402F,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
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
         USING RCONKEY,R6                                                       
         XC    KEY,KEY             READ ALL CONTRACT RECORDS                    
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,=C'CQ'                                                  
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
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PCX                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,IOAREA,DMWORK         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        MVC   P(6),=C'GETREC'                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         CLC   =C'BR',RCONTEM                                                   
         BE    PC30                                                             
         CLC   =C'EM',RCONTEM                                                   
         BE    PC30                                                             
         CLC   =C'GO',RCONTEM                                                   
         BE    PC30                                                             
         CLC   =C'LI',RCONTEM                                                   
         BNE   PCSEQ                                                            
         DROP  R6                                                               
*                                                                               
PC30     DS    0H                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   PCSEQ                                                            
*                                                                               
         USING RMODELEM,R6                                                      
         MVC   CNFDATE,RMODEL1D                                                 
         OC    RMODEL1D,RMODEL1D                                                
         BZ    PC40                                                             
         CLC   RMODEL1D,=X'C4B6'                                                
         BL    PC40                                                             
         CLC   RMODEL1D,=X'C4C8'                                                
         BH    PC40                                                             
         B     PC100                                                            
*                                                                               
PC40     DS    0H                                                               
         MVC   CNFDATE,RMODEL2D                                                 
         OC    RMODEL2D,RMODEL2D                                                
         BZ    PC50                                                             
         CLC   RMODEL2D,=X'C4B6'                                                
         BL    PC50                                                             
         CLC   RMODEL2D,=X'C4C8'                                                
         BH    PC50                                                             
         B     PC100                                                            
*                                                                               
PC50     DS    0H                                                               
         MVC   CNFDATE,RMODEL3D                                                 
         OC    RMODEL3D,RMODEL3D                                                
         BZ    PCSEQ                                                            
         CLC   RMODEL3D,=X'C4B6'                                                
         BL    PCSEQ                                                            
         CLC   RMODEL3D,=X'C4C8'                                                
         BH    PCSEQ                                                            
         DROP  R6                                                               
*                                                                               
PC100    DS    0H                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P,4                                     
         MVC   P+12(2),RCONTEM                                                  
         GOTO1 =V(DATCON),DMCB,(2,CNFDATE),(5,P+20)                             
         GOTO1 REPORT                                                           
         B     PCSEQ                                                            
         DROP  R6                                                               
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
CONKEY   DS    CL(L'KEY)                                                        
BUYKEY   DS    CL(L'KEY)                                                        
CONNUM   DS    CL4                                                              
SVCONNUM DS    CL4                                                              
MYWORK   DS    CL256                                                            
CNFDATE  DS    XL2                                                              
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
**PAN#1  DC    CL21'252REREP2402F06/11/98'                                      
         END                                                                    

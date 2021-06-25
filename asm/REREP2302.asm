*          DATA SET REREP2302  AT LEVEL 133 AS OF 05/01/02                      
*          DATA SET REREP2302  AT LEVEL 131 AS OF 12/07/99                      
*PHASE RE2302A                                                                  
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'CHECK INACTIVE STATIONS TO PURGE RATES'                         
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2302 (RE2302) --- REP FILE MARKER                   *             
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
         L     RF,=V(HELLO)                                                     
         L     RE,RELO                                                          
         AR    RF,RE                                                            
         ST    RF,VHELLO                                                        
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 DATCON,DMCB,(5,0),(19,TODAYJUL)                                  
*                                                                               
         LA    R5,0                                                             
         SHI   R5,14                                                            
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+10,(R5)                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(19,DELJUL)                              
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
*                                                                               
         MVI   KEY,X'3E'                                                        
*                                                                               
         LA    R3,RATETBL          TABLE OF VALID RATE CODES                    
         USING RATCODD,R3                                                       
*                                                                               
PCRHIGH  BAS   RE,DHIGH                                                         
         B     PC10                                                             
*                                                                               
PCRSEQ   BAS   RE,DSEQ                                                          
*                                                                               
PC10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RARTKEY,R6                                                       
*                                                                               
         CLI   RARTKTYP,X'3E'                                                   
         BNE   PC50                                                             
*                                                                               
         CLC   RARTKREP,TESTAGY                                                 
         BNE   PCRSEQ                                                           
*                                                                               
         MVC   P(2),RARTKREP                                                    
         MVC   P+3(8),RARTKCOD                                                  
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,RATEREC                                                       
         BAS   RE,DGETREC                                                       
*                                                                               
         MVC   RATREP,RARTKREP                                                  
         MVC   RATCOD,RARTKCOD                                                  
         XC    RATFLG,RATFLG                                                    
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    RATFLG,RATINA       THIS RATE CODE HAS AN INACTIVE STA           
*                                                                               
         LA    R3,RATCODLQ(R3)                                                  
*                                                                               
         L     RF,COUNT                                                         
         AHI   RF,1                                                             
         ST    RF,COUNT                                                         
*                                                                               
         B     PCRSEQ                                                           
         DROP  R3,R6                                                            
*                                                                               
PC50     DS    0H                                                               
         MVI   0(R3),X'FF'         DENOTE END OF RATETBL                        
*                                                                               
         XC    KEY,KEY                                                          
         XC    COUNT,COUNT                                                      
*                                                                               
         MVI   KEY,X'12'           LOOK UP INV RECORDS                          
*                                                                               
         BAS   RE,DHIGH                                                         
         B     PC100                                                            
*                                                                               
PCISEQ   BAS   RE,DSEQ                                                          
*                                                                               
PC100    DS    0H                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         NI    INVFLAG,X'FF'-DEL06EL                                            
*                                                                               
         CLI   RINVKTYP,X'12'                                                   
         BNE   PC200                                                            
*                                                                               
         CLC   RINVKREP,TESTAGY                                                 
         BNE   PCISEQ                                                           
**       CLC   RINVKSTA,TESTSTA                                                 
**       BNE   PCISEQ                                                           
*                                                                               
         CLI   RINVKSRC,0          WANT ONLY HEADERS NOW                        
         BNE   PCISEQ                                                           
*                                                                               
         LA    R6,INVREC                                                        
         BAS   RE,DGETREC                                                       
*                                                                               
         MVC   SAVEKEY,KEY         SAVE AWAY INVENTORY KEY                      
         MVC   SVSTA,RINVKSTA      SAVE AWAY STATION                            
         DROP  R6                                                               
*                                                                               
* CHECK '06' ELEMS IN HDR FOR RATE CODES                                        
*                                                                               
         MVI   ELCODE,X'06'        GET RATE CODE ELEMENTS                       
         BAS   RE,GETEL                                                         
         BNE   PCISEQ                                                           
         B     *+12                                                             
         USING RIMAELEM,R6                                                      
*                                                                               
PC110    BAS   RE,NEXTEL                                                        
         BNE   PC175                                                            
*                                                                               
         LA    R3,RATETBL                                                       
         USING RATCODD,R3                                                       
*                                                                               
         MVI   STATFLAG,0          CLEAR STATUS FLAG                            
*                                                                               
PC120    DS    0H                                                               
         CLI   0(R3),X'FF'         IS CODE IN TABLE?                            
         BE    PC130               NO - SAVE EQU # SO WE CAN DELETE (Z)         
         CLC   RATREP,RIMAREP      SAME AGY?                                    
         BNE   PC150                                                            
         CLC   RATCOD,RIMACDE      SAME CODE?                                   
         BNE   PC150                                                            
*                                                                               
*!!      MVC   P(10),=C'VALID RATE'                                             
         OI    STATFLAG,RATEFND    FOUND MATCH, CHECK INACTIVE                  
         B     *+14                                                             
*                                                                               
PC130    MVC   P(8),=C'NO MATCH'   NO MATCH FOUND IN TABLE                      
         OI    INVFLAG,PURGERAT    DELETE RATE FROM INV REC                     
*                                                                               
         MVC   SVRCREP,RIMAREP                                                  
         MVC   SVRCDE,RIMACDE                                                   
         MVC   EQUATE,RIMANUM                                                   
*                                                                               
         MVC   P+45(2),RIMAREP                                                  
         MVC   P+48(8),RIMACDE                                                  
         EDIT  RIMANUM,(1,P+57)                                                 
         B     PC135                                                            
*                                                                               
PC135    DS    0H                                                               
         MVC   P+15(2),INVREC+10                                                
         MVC   P+18(5),INVREC+12                                                
         MVC   P+24(4),INVREC+17                                                
         GOTO1 DATCON,DMCB,(3,INVREC+21),(5,P+30)                               
*                                                                               
         TM    STATFLAG,RATEFND    CHECK INACTIVE?                              
         BZ    *+8                                                              
         BAS   RE,PROCRAT          PROCESS THIS RATE CODE FOR INV REC           
         NI    STATFLAG,X'FF'-RATEFND                                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         TM    INVFLAG,PURGERAT    PURGE RATES FOR THIS INV REC?                
         BZ    PC140                                                            
         BAS   RE,DELRATES                                                      
*                                                                               
*!!!     MVC   P(11),=C'REM 06 ELEM'                                            
*                                                                               
         MVC   RIMACDE(2),=XL2'FFFF'  MARK THIS '06' TO DELETE LATER            
         OI    INVFLAG,DEL06EL                                                  
*                                                                               
         ST    R6,MYSVADDR                                                      
*                                                                               
         LA    R6,INVREC                                                        
         BAS   RE,DPUTREC                                                       
*                                                                               
         L     R6,MYSVADDR                                                      
*&&DO                                                                           
         MVC   P+39(2),=C'FF'                                                   
*                                                                               
         EDIT  RIMANUM,(2,P+42)                                                 
         MVC   P+45(2),RIMAREP                                                  
         MVC   P+48(8),RIMACDE                                                  
         GOTO1 REPORT                                                           
*&&                                                                             
*                                                                               
PC140    DS    0H                                                               
         NI    INVFLAG,X'FF'-PURGERAT                                           
*                                                                               
         LA    R3,RATETBL                                                       
         B     PC110                                                            
*                                                                               
PC150    LA    R3,RATCODLQ(R3)                                                  
         B     PC120                                                            
         DROP  R3,R6                                                            
*                                                                               
PC175    DS    0H                                                               
         TM    INVFLAG,DEL06EL                                                  
         BZ    PCISEQ                                                           
*                                                                               
         BAS   RE,REM06EL          DELETE THE MARKED 06 ELEMENTS                
         B     PCISEQ                                                           
*                                                                               
*&&DO                                                                           
         LA    R6,INVREC                                                        
         GOTO1 VHELLO,DMCB,(C'D',REPFILE),(X'FF',(R6)),0,0                      
         BAS   RE,DPUTREC                                                       
         B     PCISEQ                                                           
*&&                                                                             
PC200    DS    0H                                                               
         BAS   RE,ADDRCD05         ADD 05 ELEMS TO RATE CODE RECS               
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(14),=C'MADE IT TO PCX'                                         
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
******************************************************************              
*        DELETE MARKED 06 ELEMENTS IN INV HEADER                                
******************************************************************              
REM06EL  NTR1                                                                   
         LA    R6,INVREC                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         DC    H'00'                                                            
         USING RIMAELEM,R6                                                      
*                                                                               
REM0610  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   REM0650             NO MORE 06 ELEMENTS                          
*                                                                               
         CLC   RIMACDE(2),=XL2'FFFF'  MARKED DELETED?                           
         BNE   REM0610                                                          
*                                                                               
         MVI   RIMACODE,X'FF'                                                   
*                                                                               
         MVC   P(11),=C'REM 06 ELEM'                                            
         MVC   P+39(2),=C'FF'                                                   
         EDIT  RIMANUM,(2,P+42)                                                 
         MVC   P+45(2),RIMAREP                                                  
         MVC   P+48(8),RIMACDE                                                  
         GOTO1 REPORT                                                           
*                                                                               
         B     REM0610                                                          
*                                                                               
REM0650  DS    0H                                                               
         LA    R6,INVREC                                                        
         GOTO1 VHELLO,DMCB,(C'D',REPFILE),(X'FF',(R6)),0,0                      
         BAS   RE,DPUTREC                                                       
*                                                                               
REM06ELX DS    0H                                                               
         B     DXIT1                                                            
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*        ADD 05 ELEMS TO RATE CODE RECORD                                       
******************************************************************              
ADDRCD05 NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'3E'                                                        
*                                                                               
         BAS   RE,DHIGH                                                         
         B     ADRC0510                                                         
*                                                                               
ADDRCSEQ BAS   RE,DSEQ                                                          
*                                                                               
ADRC0510 DS    0H                                                               
         LA    R6,KEY                                                           
         USING RARTKEY,R6                                                       
*                                                                               
         MVC   SVRCDE,RARTKCOD                                                  
*                                                                               
         CLI   RARTKTYP,X'3E'                                                   
         BNE   ADDRC05X                                                         
*                                                                               
         CLC   RARTKREP,TESTAGY                                                 
         BNE   ADDRCSEQ                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R6,RATEREC                                                       
         BAS   RE,DGETREC                                                       
*                                                                               
ADRC0515 DS    0H                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDRCSEQ                                                         
         B     *+12                                                             
         USING RAIACODE,R6                                                      
*                                                                               
ADRC0520 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   ADDRCSEQ                                                         
*                                                                               
         CLI   RAIALUID+8,X'80'    DELETE THIS 04 ELEMENT?                      
         BNE   ADRC0520                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R3,ELEM                                                          
         USING RAPUELEM,R3                                                      
*                                                                               
         MVI   RAPUCODE,X'05'                                                   
         MVI   RAPULEN,RAPULENQ                                                 
         MVC   RAPUSTA,RAIASTA                                                  
         MVC   RAPUCDTE,TODAYJUL                                                
         MVC   RAPULUID,RAIALUID                                                
*                                                                               
         MVI   0(R6),X'FF'         DELETE THIS INACTIVE ELEMENT                 
         LA    R6,RATEREC                                                       
         GOTO1 VHELLO,DMCB,(C'D',REPFILE),(X'FF',(R6)),0,0                      
*                                                                               
         MVC   P(11),=C'ADD 05 ELEM'                                            
         MVC   P+15(8),SVRCDE                                                   
         MVC   P+25(5),RAPUSTA                                                  
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',REPFILE),(0,(R6)),ELEM,0                       
*                                                                               
         BAS   RE,DPUTREC                                                       
         B     ADRC0515                                                         
*                                                                               
ADDRC05X DS    0H                                                               
         B     DXIT1                                                            
         DROP  R3,R6                                                            
         EJECT                                                                  
******************************************************************              
*        DELETE RATES FOR THIS INV RECORD                                       
******************************************************************              
DELRATES NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   KEY(24),SAVEKEY                                                  
         MVI   RINVKSRC,C'Z'                                                    
         MVC   RINVKNUM,EQUATE                                                  
*                                                                               
         BAS   RE,DHIGH                                                         
         B     *+8                                                              
*                                                                               
DELRSEQ  BAS   RE,DSEQ                                                          
*                                                                               
DELR10   DS    0H                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   DELR100                                                          
*                                                                               
         LA    R6,INVREC           DELETE THIS RATE RECORD                      
         BAS   RE,DGETREC                                                       
*                                                                               
         MVC   P(11),=C'DEL RDETAIL'                                            
*                                                                               
         MVC   P+15(2),RINVKREP                                                 
         MVC   P+18(5),RINVKSTA                                                 
         MVC   P+24(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+30)                                
         EDIT  RINVKNUM,(1,P+40)                                                
         EDIT  RINVKYR,(2,P+42)                                                 
         GOTO1 REPORT                                                           
*                                                                               
         OI    KEY+27,X'80'                                                     
         BAS   RE,DWRT                                                          
         OI    29(R6),X'80'                                                     
         BAS   RE,DPUTREC                                                       
*                                                                               
         B     DELRSEQ                                                          
*                                                                               
DELR100  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         BAS   RE,DHIGH                                                         
         LA    R6,INVREC                                                        
         BAS   RE,DGETREC                                                       
*                                                                               
DELRATEX DS    0H                                                               
         B     DXIT1                                                            
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*        PROCESS THIS RATE CODE FOR INV REC                                     
******************************************************************              
PROCRAT  NTR1                                                                   
         XC    KEY,KEY                                                          
         NI    INVFLAG,X'FF'-PURGERAT                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RARTKEY,R6                                                       
*                                                                               
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,SVRCREP                                                 
         MVC   RARTKCOD,SVRCDE                                                  
*                                                                               
         BAS   RE,DHIGH                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R6,RATEREC                                                       
         BAS   RE,DGETREC                                                       
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROCR30                                                          
         B     *+8                                                              
         USING RAIAELEM,R6                                                      
*                                                                               
PROCR20  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PROCR30                                                          
*                                                                               
         CLC   RAIASTA,SVSTA       IS STATION INACTIVE?                         
         BNE   PROCR20                                                          
*                                                                               
         CLC   RAIACDTE,DELJUL     WITHIN 2 WEEKS OF INACTIVITY?                
         BNL   PROCR20                                                          
*                                                                               
         MVI   RAIALUID+8,X'80'    MARK THIS ELEMENT FOR DELETION               
*                                                                               
         OI    INVFLAG,PURGERAT    PURGE THIS RATE FOR INV RECORD               
*                                                                               
         LA    R6,RATEREC                                                       
         BAS   RE,DPUTREC                                                       
*                                                                               
PROCR30  DS    0H                  RESTORE INVREC INFO                          
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         BAS   RE,DHIGH                                                         
         LA    R6,INVREC                                                        
         BAS   RE,DGETREC                                                       
*                                                                               
PROCRATX DS    0H                                                               
         MVI   ELCODE,X'06'                                                     
         B     DXIT1                                                            
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*        DATAMGR CALLS                                                          
******************************************************************              
DHIGH    NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'00'                                                            
DHIGHX   B     DXIT1                                                            
*                                                                               
DSEQ     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'00'                                                            
DSEQX    B     DXIT1                                                            
*                                                                               
DGETREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,(R6),DMWORK               
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
DGETRECX B     DXIT1                                                            
*                                                                               
DPUTREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,(R6),DMWORK               
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
DPUTRECX B     DXIT1                                                            
*                                                                               
DWRT     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY                            
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
DWRTX    B     DXIT1                                                            
*                                                                               
DXIT1    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
EXIT     DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
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
VHELLO   DS    F                                                                
*                                                                               
MYSVADDR DS    F                                                                
*                                                                               
TODAYJUL DS    XL3                 TODAY'S DATE IN JULIAN                       
DELJUL   DS    XL3                 DELETE DATE IN JULIAN                        
EQUATE   DS    XL1                 EQUATE # OF (Z) RECORDS                      
*                                                                               
SVRCREP  DS    CL2                 SAVED RATE CODE REP                          
SVRCDE   DS    CL8                 SAVED RATE CODE                              
SVSTA    DS    CL5                 SAVED STATION                                
*                                                                               
STATFLAG DS    XL1                 STATUS FLAGS                                 
RATEFND  EQU   X'01'               RATE IN TBLE & REC, CHECK INACTIVE           
*                                                                               
INVFLAG  DS    XL1                 INV REC FALGS                                
PURGERAT EQU   X'01'               PURGE THIS RATE FROM THE INV RECORD          
DEL06EL  EQU   X'02'               DELETE 06 ELEM IN HEADER                     
*                                                                               
TESTAGY  DC    CL2'B3'             EJOR                                         
TESTSTA  DC    CL5'KMOLT'                                                       
TESTRCDE DC    CL8'MTL'                                                         
*                                                                               
         DC    CL8'SAVEKEY'                                                     
SAVEKEY  DS    XL27                                                             
*                                                                               
         DC    CL8'ELEM'                                                        
ELEM     DS    XL50                                                             
ELCODE   DS    XL1                                                              
*                                                                               
         DC    CL8'EQU#TBL'                                                     
EQU#TBL  DS    XL100                                                            
*                                                                               
         DC    CL8'RATETBL'                                                     
RATETBL  DS    XL2000                                                           
*                                                                               
         DC    CL8'RATEREC'                                                     
RATEREC  DS    XL2000                                                           
*                                                                               
         DC    CL8'INVREC'                                                      
INVREC   DS    XL2000                                                           
*                                                                               
******************************************************************              
*        DSECTS                                                                 
******************************************************************              
RATCODD  DSECT                                                                  
RATREP   DS    CL2                 AGY                                          
RATCOD   DS    CL8                 RATE CODE                                    
RATFLG   DS    XL1                 FLAGS                                        
RATINA   EQU   X'01'               CONTAINS INACTIVE STATION                    
RATCODLQ EQU   *-RATREP                                                         
*                                                                               
*                                                                               
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
       ++INCLUDE REGENARTE                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'133REREP2302 05/01/02'                                      
         END                                                                    

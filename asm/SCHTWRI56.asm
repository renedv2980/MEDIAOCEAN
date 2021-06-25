*          DATA SET SCHTWRI56  AT LEVEL 052 AS OF 05/01/02                      
*PHASE T32056A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HELLO                                                                  
         TITLE 'T32056 - PROGRAM REC DELETE REPORT'                             
                                                                                
***************************************************************                 
*                                                                               
*        ALLOWS USER TO DELETE ONE OR ALL PROGRAM CODES                         
*        FOR A SPECIFIED NETWORK/DATE RANGE (JUN00 - SCHT)                      
*                                                                               
***************************************************************                 
T32056   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEPRGD,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2          ANETWS2+300=WORKING STORAGE                  
         A     R7,=F'300'                                                       
         USING WORKD,R7                                                         
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNET,DMCB,MKTNUMB       MKT NUMBER SET TO MKTNUMB               
*                                                                               
         LA    R2,SPLEDTH               END DATE                                
         XC    PDATSTRT,PDATSTRT                                                
         XC    PDATEND,PDATEND                                                  
*                                                                               
         GOTO1 PERVAL,DMCB,(SPLEDTH+5,SPLEDT),MYDMWRK                           
         TM    DMCB+4,X'04'        ONLY 1 DATE INPUT                            
         BZ    EDINV                                                            
*                                                                               
         LA    RF,MYDMWRK                                                       
         USING PERVALD,RF                                                       
*                                                                               
         MVC   PDATEND,PVALCEND    END OF PERIOD                                
         DROP  RF                                                               
*                                                                               
         LA    RE,PROGTBL                                                       
         L     RF,=F'50000'                                                     
         XCEF                                                                   
*                                                                               
         LA    R2,SPLPRGH               PROGRAM                                 
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   EDT50                                                            
*                                                                               
EDT20    DS    0H                  GET ALL PROG RECS (BUILD PROGTBL)            
         XC    COUNTER,COUNTER                                                  
         LA    R4,PROGTBL                                                       
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING NPGRECD,RF                                                       
*                                                                               
         MVC   KEY(2),=X'0D20'                                                  
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,MKTNUMB                                                  
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     EDT25                                                            
*                                                                               
EDTSEQ   DS    0H                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
EDT25    CLC   KEY(5),KEYSAVE            SAME NETWORK?                          
         BNE   EDT40                     NO - DONE                              
*                                                                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    RF,KEY                                                           
         CLI   NPGKPROG,0                                                       
         BE    EDTSEQ                                                           
*                                                                               
         CLC   NPGKEND,PDATEND                                                  
         BH    EDTSEQ              PROG DATE IS > THAN REQ. DATE                
*                                                                               
EDT35    DS    0H                                                               
         LA    RF,KEY                                                           
         MVC   0(6,R4),NPGKPROG                                                 
         MVC   8(2,R4),NPGKEND                                                  
         DROP  RF                                                               
*                                                                               
         L     R2,AIO                                                           
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDT37                                                            
*                                                                               
         OC    2(2,R2),2(R2)       ANY START DATE                               
         BZ    *+10                                                             
         MVC   6(2,R4),2(R2)                                                    
*                                                                               
EDT37    LA    R4,11(R4)                                                        
*                                                                               
         L     RF,COUNTER                                                       
         AHI   RF,1                                                             
         ST    RF,COUNTER                                                       
*                                                                               
         B     EDTSEQ                                                           
*                                                                               
EDT40    DS    0H                                                               
         XC    FILENAME,FILENAME         YES/RESET TO UNIT FILE                 
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         MVI   0(R4),X'FF'         DENOTE END OF TABLE                          
         B     EDT60                                                            
*                                                                               
*        CHECK IF PRGRAM REC EXISTS                                             
*                                                                               
EDT50    DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         OC    SPLPRG,SPACES      NUKPROG HAS SPACES AT END                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),MKTNUMB                                                 
         MVC   KEY+5(6),SPLPRG                                                  
         MVC   KEY+11(2),PDATEND                                                
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE           DOES PROGRAM REC EXIST ?               
         BNE   EDINV                     NO/ERROR                               
*                                                                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         LA    RF,KEY                                                           
         USING NPGRECD,RF                                                       
*                                                                               
         LA    R4,PROGTBL                                                       
         MVC   0(6,R4),NPGKPROG                                                 
         MVC   8(2,R4),NPGKEND                                                  
         DROP  RF                                                               
*                                                                               
         L     R2,AIO                                                           
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDT60                                                            
*                                                                               
         OC    2(2,R2),2(R2)       ANY START DATE                               
         BZ    *+10                                                             
         MVC   6(2,R4),2(R2)                                                    
*                                                                               
         MVI   11(R4),X'FF'                                                     
*                                                                               
EDT60    XC    FILENAME,FILENAME         YES/RESET TO UNIT FILE                 
         NETGO NVSETUNT,DMCB                                                    
*                                                                               
         MVI   TESTRUN,C'Y'                                                     
         LA    R2,SPLTESTH        TEST RUN                                      
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLI   SPLTEST,C'Y'                                                     
         BE    EDTX                                                             
         CLI   SPLTEST,C'N'        LIVE RUN                                     
         BNE   EDINV                                                            
         MVI   TESTRUN,C'N'                                                     
*                                                                               
EDTX     LA    R2,SPLNETH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
* - REPORT MODE                                                                 
*                                                                               
* - READ THROUGH UNITS TO ENSURE NONE EXIST WITH PROGRAM REC                    
* - TO BE DELETED                                                               
*                                                                               
REPMOD   NTR1                                                                   
         LA    R2,KEY                                                           
         USING NURECD,R2                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           UNIT ID                                      
         MVC   KEY+1(1),NBACTAM                                                 
         GOTO1 HIGH                                                             
         B     REP10                                                            
*                                                                               
REPSEQ   GOTO1 SEQ                                                              
*                                                                               
REP10    CLC   KEY(2),KEYSAVE                                                   
         BNE   REP30                                                            
         CLC   NUKPROG,SPLPRG                                                   
         BNE   REPSEQ                                                           
         CLC   NUKDATE,PDATEND                                                  
         BNL   REPSEQ                                                           
*                                                                               
* FOUND A UNIT WITHIN REQUESTED DELETE PERIOD                                   
         MVC   P(17),=C'***** ERROR *****'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 CLUNPK,DMCB,NUKCLT,P+2                                           
         EDIT  (B1,NUKEST),(3,P2+2)                                             
         MVC   P3+2(4),NUKNET                                                   
         MVC   P4+2(6),NUKPROG                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 DATCON,DMCB,(2,NUKDATE),(5,P+2)                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XIT1                    AND THAT'S ALL                                 
         EJECT                                                                  
*                                                                               
* NO UNITS WITHIN REQUESTED PROGRAM REC DELETE PERIOD                           
REP30    DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'SPTDIR  '                                            
         USING NPGRECD,R2                                                       
*                                                                               
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,MKTNUMB                                                  
         MVC   NPGKPROG,SPLPRG                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     REP40                                                            
*                                                                               
REP35    MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
         B     REP40                                                            
*                                                                               
REP40    CLC   KEY(11),KEYSAVE                                                  
         BNE   REPX                                                             
         CLC   NPGKEND,PDATEND     IS REC DATE WITHIN REQUEST PERIOD            
         BH    REP35                      NO/GO SEQ                             
*                                                                               
         MVC   FILENAME,=C'SPTFIL  '      YES/GET REC AND DELETE IT             
         GOTO1 GETREC                                                           
         MVI   ACTELOPT,C'N'       DON'T WRITE ACTIVITY ELEM                    
*                                  GENCON USES SYSFIL AND THUS THINKS           
*                                  IT IS DEALING WITH UNIT REC SO GETS          
*                                  INCORRECT DISPLACEMENT TO 1ST ELEM           
         L     R2,AIO                                                           
         OI    NPGCNTL,X'80'       DELETE RECORD                                
         CLI   TESTRUN,C'Y'                                                     
         BNE   REP50                                                            
         BAS   RE,PRINTREC                                                      
         B     REP55                                                            
*                                                                               
REP50    GOTO1 PUTREC                                                           
         BAS   RE,PRINTREC                                                      
*                                  DELETE KEY                                   
REP55    OI    KEY+13,X'80'                                                     
         CLI   TESTRUN,C'Y'                                                     
         BNE   REP60                                                            
***      GOTO1 HEXOUT,DMCB,KEY,P,15,0                                           
***      GOTO1 SPOOL,DMCB,(R8)                                                  
         B     REP65                                                            
REP60    GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
*                                                                               
REP65    B     REP35               GET NEXT RECORD                              
*                                                                               
REPX     XIT1                                                                   
*                                                                               
PRINTREC NTR1                                                                   
         MVC   P+1(6),NPGKPROG                                                  
         GOTO1 DATCON,DMCB,(2,NPGKEND),(5,P+8)                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
HEADSPC  SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,46,C'PROGRAM RECORD DELETION REPORT'                          
         SSPEC H2,46,C'______________________________'                          
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,1,C'NETWORK'                                                  
         SSPEC H4,1,C'PROGRAM'                                                  
         SSPEC H5,1,C'END DATE'                                                 
         SSPEC H4,99,REPORT                                                     
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HOOK     NTR1                                                                   
         MVC   H3+10(8),SPLNET                                                  
         MVC   H4+10(6),SPLPRG                                                  
         MVC   H5+10(9),SPLEDT                                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*        WORKING STORAGE                                                        
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
MKTNUMB  DS    H                   NETWORK MARKET NUMBER                        
PDATSTRT DS    CL2                 PROGRAM REC START DATE COMPRESSED            
PDATEND  DS    CL2                 PROGRAM REC END DATE COMPRESSED              
TESTRUN  DS    CL1                                                              
*                                                                               
IOAREA   DS    CL2000                                                           
         DC    CL7'PROGTBL'                                                     
PROGTBL  DS    CL50000             TABLE FOR ALL VALID PROGRAM CODES            
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SCHTWRI56 05/01/02'                                      
         END                                                                    

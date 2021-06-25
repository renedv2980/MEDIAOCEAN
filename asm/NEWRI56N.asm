*          DATA SET NEWRI56N   AT LEVEL 012 AS OF 05/01/02                      
*          DATA SET NEWRI56    AT LEVEL 019 AS OF 01/23/98                      
*PHASE T32056A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HELLO                                                                  
         TITLE 'T32056 - PROGRAM REC DELETE REPORT'                             
                                                                                
*****************************************************                           
*  OPTIONS:                                                                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*******************************************************                         
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
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNET,DMCB,MKTNUMB       MKT NUMBER SET TO MKTNUMB               
*                                                                               
         MVI   TESTRUN,C'Y'                                                     
         LA    R2,SPLTESTH        TEST RUN                                      
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLI   SPLTEST,C'Y'                                                     
         BE    EDIT05                                                           
         CLI   SPLTEST,C'N'        LIVE RUN                                     
         BNE   EDINV                                                            
         MVI   TESTRUN,C'N'                                                     
*                                                                               
EDIT05   LA    R2,SPLPRGH               PROGRAM                                 
         CLI   5(R2),0                                                          
         BNH   EDINV                                                            
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO SPOT FILE                             
         LA    R4,PROGTBL          GET PROGRAM TABLE                            
         USING PROGTBLD,R4                                                      
                                                                                
         LA    R3,3                3 PROG/DATE GROUPS PER SCREEN LINE           
EDIT10   LR    R5,R2                SAVE FOR ERROR MESSAGE                      
         CLI   5(R2),0              EMPTY?                                      
         BNE   EDT12                                                            
         BAS   RE,BUMPFLD           YES/BUMP TO NEXT PROG/DATE                  
         BAS   RE,BUMPFLD                                                       
         B     EDT20                                                            
EDT12    MVC   PROGCOD,8(R2)        SET PROGRAM CODE                            
         BAS   RE,BUMPFLD                                                       
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    EDINV                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,PROGDAT) PROGRAM REC END DATE            
         BAS   RE,CHKPRGRC         CHECK IF PROG REC EXISTS                     
         BNE   PROGINV                                                          
         LA    R4,8(R4)            BUMP PROGRAM/DATE TABLE                      
         BAS   RE,BUMPFLD          BUMP TO NEXT PROG SCREEN FIELD               
EDT20    BCT   R3,EDIT10                                                        
         LA    R3,3                SET UP FOR NEXT LINE                         
         B     EDIT10                                                           
                                                                                
CHKPRGRC NTR1                                                                   
*        CHECK IF PRGRAM REC EXITST                                             
                                                                                
         OC    PROGCOD,SPACES      NUKPROG HAS SPACES AT END                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),MKTNUMB                                                 
         MVC   KEY+5(6),PROGCOD                                                 
         MVC   KEY+11(2),PROGDAT                                                
                                                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE           DOES PROGRAM REC EXIST ?               
         XIT1                                                                   
*                                                                               
                                                                                
*                                                                               
EDTX     LA    R2,SPLNETH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
BUMPFLD  ZIC   R1,0(R2)            GET LENGTH                                   
         CH    R1,9                                                             
         BNH   EDTX                THAT'S ALL                                   
         AR    R2,R1               TO NEXT FIELD                                
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    BUMPFLD             YES/SKIP                                     
         BR    RE                  RETURN TO CALLER                             
                                                                                
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
PROGINV  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(25),=C'*** ERROR INVALID PROGRAM'                        
         OI    CONHEADH+6,X'80'                                                 
         LR    R2,R5               CURSOR TO PROGRAM SCREEN FIELD               
         GOTO1 ERREX2                                                           
                                                                                
         EJECT                                                                  
* - REPORT MODE                                                                 
*                                                                               
* - READ THROUGH UNITS TO ENSURE NONE EXIST WITH PROGRAM REC                    
* - TO BE DELETED                                                               
                                                                                
REPMOD   NTR1                                                                   
                                                                                
                                                                                
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         LA    R2,KEY                                                           
         USING NURECD,R2                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           UNIT ID                                      
         MVC   KEY+1(1),NBACTAM                                                 
         GOTO1 HIGH                                                             
         B     REP10                                                            
                                                                                
REPSEQ   GOTO1 SEQ                                                              
                                                                                
REP10    CLC   KEY(2),KEYSAVE                                                   
         BNE   REP30                                                            
                                                                                
* TEST IF ANY UNITS HAVE PROGRAM CODES SET FOR DELETION                         
         LA    R3,PROGTBL                                                       
         LA    R4,18               MAX NUMBER OF PROGRAMS/END DATES             
REP12    CLC   NUKPROG,0(R3)       PROGRAM ?                                    
         BNE   REP14               NO                                           
         CLC   NUKDATE,6(R3)       YES/ WITHIN DATE PERIOD?                     
         BL    REP15               YES-ERROR                                    
REP14    LA    R3,8(R3)            NO/BUMP TO NEXT PROG CODE                    
         CLI   0(R3),0                                                          
         BE    REPSEQ                                                           
         BCT   R4,REP12                                                         
         B     REPSEQ              NO MATCH/GET NEXT UNIT                       
                                                                                
* FOUND A UNIT WITHIN REQUESTED DELETE PERIOD                                   
REP15    MVC   P(17),=C'***** ERROR *****'                                      
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
                                                                                
* NO UNITS WITHIN REQUESTED PROGRAM REC DELETE PERIOD                           
REP30    DS    0H                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'SPTDIR  '                                            
         USING NPGRECD,R2                                                       
                                                                                
         LA    R3,PROGTBL          TABLE OF PRGS TO DELETE                      
         LA    R4,18               MAX NUMBER OF PROGRAMS                       
REP32    XC    KEY,KEY                                                          
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,MKTNUMB                                                  
         MVC   NPGKPROG,0(R3)                                                   
                                                                                
         GOTO1 HIGH                                                             
         B     REP40                                                            
                                                                                
REP35    MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
         B     REP40                                                            
                                                                                
REP40    CLC   KEY(11),KEYSAVE                                                  
         BNE   REPX                                                             
         CLC   NPGKEND,6(R3)       IS REC DATE WITHIN REQUEST PERIOD            
         BH    REP35                      NO/GO SEQ                             
                                                                                
         MVC   FILENAME,=C'SPTFIL  '      YES/GET REC AND DELETE IT             
         GOTO1 GETREC                                                           
         MVI   ACTELOPT,C'N'       DON'T WRITE ACTIVITY ELEM                    
*                                  GENCON USES SYSFIL AND THUS THINKS           
*                                  IT IS DEALING WITH UNIT REC SO GETS          
*                                  INCORRECT DISPLACEMENT TO 1ST ELEM           
         L     R2,AIO                                                           
         BAS   RE,PRINTREC         PRINT RECORD                                 
         CLI   TESTRUN,C'Y'        TEST?                                        
         BE    REP35               YES/GET NEXT RECORD                          
                                                                                
         OI    NPGCNTL,X'80'       NO/DELETE RECORD                             
         GOTO1 PUTREC                                                           
*                                  DELETE KEY                                   
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         B     REP35               GET NEXT RECORD                              
*                                                                               
REPX     LA    R3,8(R3)            BUMP TO NEXT PROGRAM RECORD                  
         CLI   0(R3),0                                                          
         BE    REPXX                                                            
         BCT   R4,REP32                                                         
REPXX    XIT1                                                                   
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
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVC   H3+10(8),SPLNET                                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
MKTNUMB  DS    H                   NETWORK MARKET NUMBER                        
TESTRUN  DS    CL1                                                              
PROGTBL  DS    CL264               FOR 33 RECS X 8                              
                                                                                
PROGTBLD DSECT                                                                  
PROGCOD  DS    CL6                                                              
PROGDAT  DS    CL2                 PROGRAM REC END DATE COMPRESSED              
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEWRI56N  05/01/02'                                      
         END                                                                    

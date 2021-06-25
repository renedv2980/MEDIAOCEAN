*          DATA SET PEMSG04    AT LEVEL 080 AS OF 05/01/02                      
*PHASE TE1D04A                                                                  
         TITLE 'TE1D04 - WRITE REPORTS'                                         
TE1D04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TE1D04                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TE1DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,KEY                                                           
         USING MSGKEYD,R4                                                       
         EJECT                                                                  
*              CHECK ON/OFF LINE AND SET UP KEY                                 
         SPACE 3                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   MAKEKEY                                                          
*                                                                               
         LA    R2,REPNAMH                                                       
         GOTO1 ANY1                                                             
         MVC   SGNONUN,WORK                                                     
         LA    R2,REPFILH                                                       
         GOTO1 ANY1                                                             
         MVC   OPNFILE,WORK                                                     
*                                    GET USER OPTIONS                           
         XC    KEY,KEY                                                          
         MVI   MSGSYS,MSGSYSQ                                                   
         MVC   MSGAGY,TWAORIG                                                   
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         MVC   USRNAME,SGNONUN                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         MVC   SGNONOP,4(R6)                                                    
*                                                                               
MAKEKEY  XC    KEY,KEY                                                          
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,MSGKTYPQ                                                
         MVC   MSGKFILE,OPNFILE                                                 
         EJECT                                                                  
*              SET UP WHEN                                                      
         SPACE 3                                                                
         CLI   REPCMMDH+5,0                                                     
         BE    EXIT                                                             
         MVC   REMUSER,=C'MSG'                                                  
         MVI   TWAWHEN,0                                                        
         XC    TWAOUT,TWAOUT                                                    
         XC    TWADEST,TWADEST                                                  
         MVI   WHEN,X'40'                                                       
         CLI   REPCMMD,C'N'                                                     
         BE    REPT                                                             
*                                                                               
         CLI   REPDAP,C'Y'                                                      
         BNE   M1                                                               
         MVC   SGNMSG(25),=C'OPTION ONLY AVAILABLE NOW'                         
         OI    REPDAPH+6,X'40'                                                  
         B     EXIT1                                                            
*                                                                               
M1       MVI   TWAWHEN,2                                                        
         MVI   WHEN,X'20'                                                       
         CLI   REPCMMD,C'S'                                                     
         BE    REPT                                                             
         MVI   TWAWHEN,4                                                        
         MVI   WHEN,X'10'                                                       
         CLI   REPCMMD,C'O'                                                     
         BE    REPT                                                             
         MVI   WHEN,X'08'                                                       
         CLI   REPCMMD,C'D'                                                     
         BE    REPT                                                             
         MVC   SGNMSG(21),=C'OPTION NOT RECOGNIZED'                             
EXIT     OI    REPCMMDH+6,X'40'                                                 
EXIT1    OI    SGNMSGH+6,X'80'                                                  
         OI    REPCMMDH+6,X'80'                                                 
         MVC   REPCMMD,SPACES                                                   
         B     XIT                                                              
         EJECT                                                                  
*              MAINTENANCE ROUTINES - REPORT                                    
         SPACE 1                                                                
REPT     CLI   TWAWHEN,0           IF OVERNIGHT OR SOON WAS REQUESTED           
         BE    REP4                   WRITE OUT REQUESTS NOW                    
         CLI   OFFLINE,C'Y'        UNLESS WE ARE OFFLINE NOW                    
         BE    REP4                                                             
****** BUILD REQUEST SCREEN HERE                                                
         MVC   REPNAM,SGNONUN                                                   
         MVI   REPNAMH+5,8                                                      
         MVC   REPFIL,OPNFILE                                                   
         MVI   REPFILH+5,8                                                      
         OI    REPNAMH+6,X'80'                                                  
         OI    REPFILH+6,X'80'                                                  
******* END BUILD                                                               
         MVC   RCPROG+2(2),=C'MS'                                               
         MVC   QCRDCODE(2),=C'MS'                                               
         GOTO1 BLDREQST                                                         
         B     EXIT                                                             
         SPACE 1                                                                
REP4     CLI   TWAFIRST,0          FIRST TIME HOOK                              
         BNE   REP5                                                             
         MVI   TWAFIRST,1                                                       
***** INSERT FIRST TIME CODE HERE                                               
         B     REP5B                                                            
         SPACE 1                                                                
REP5     CLI   TWAFIRST,X'FF'       LAST TIME HOOK                              
         BNE   REP5B                                                            
***** INSERT LAST TIME CODE HERE                                                
         B     EXIT                                                             
         SPACE 1                                                                
REP5B    MVC   REPE1,SPACES                                                     
         MVC   REPE2,SPACES                                                     
***** USING EXTRA PRINT QUEUE SETUP OPTIONS                                     
         L     RE,AIO2                                                          
         XC    0(128,RE),0(RE)                                                  
         ST    RE,SPOOLQLK                                                      
         OI    SPOOLIND,X'40'                                                   
         USING PQPLD,RE                                                         
         MVI   QLEXTRA,X'FF'                                                    
***** ALL SET UP, NOW USE                                                       
         MVC   QLCLASS,SGNONMC  MESSAGE CLASS                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 STRTPRNT            OPEN PRINT INTERVAL                          
***** PRINT CODE GOES HERE                                                      
*              PRINT REPORT                                                     
         SPACE 3                                                                
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
REPPLP   CLC   KEY(21),KEYSAVE                                                  
         BNE   REP8A                                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'34'                                                     
         BAS   RE,GETEL                                                         
         MVI   COPYNO,1                                                         
         BNE   *+10                                                             
         MVC   COPYNO,2(R6)                                                     
         MVI   COPYFLG,1 FIRST TIME                                             
         LA    R3,43 LINE CTR                                                   
         B     REPP2                                                            
*                                                                               
REPP1    GOTO1 GETREC                                                           
*                                                                               
REPP2    LA    R2,1  EL PTR                                                     
REPP2A   L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
REPP2D   BNE   REPP2B                                                           
         ZIC   R1,2(R6)                                                         
         CR    R1,R2                                                            
         BE    REPP2C                                                           
         BAS   RE,NEXTEL                                                        
         B     REPP2D                                                           
REPP2C   CLI   1(R6),4                                                          
         BE    REPP2C1                                                          
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXMVC R1,P+3,4(R6)                                                     
         TM    3(R6),X'20'                                                      
         BZ    REPP2C0                                                          
         LA    R1,90                                                            
REPP2C00 LA    R5,P(R1)                                                         
         CLI   0(R5),C'#'                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         BCT   R1,REPP2C00                                                      
REPP2C0  CLC   P+3(2),=C'$P'                                                    
         BE    REPP2NP                                                          
         CLC   P+3(2),=X'5B97'     $,LCP                                        
         BE    REPP2NP                                                          
REPP2C1  GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,1(R2)                                                         
         BCT   R3,REPP2A                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,43 LINE CTR                                                   
         B     REPP2A                                                           
REPP2NP  LA    R2,1(R2)                                                         
         MVC   P,SPACES                                                         
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,43 LINE CTR                                                   
         B     REPP2A                                                           
*                                                                               
REPP2B   GOTO1 SEQ                                                              
         CLC   KEY(35),KEYSAVE                                                  
         BE    REPP1    SAME MSG                                                
         MVI   FORCEHED,C'Y'                                                    
         ZIC   R1,COPYNO                                                        
         BCTR  R1,0                                                             
         STC   R1,COPYNO                                                        
         CLI   COPYNO,0  ANOTHER COPY?                                          
         BE    REPPLP NO                                                        
         L     R6,AIO                                                           
         MVC   KEY,0(R6)                                                        
         MVI   MSGKPAGE,0 YES,BACK TO FIRST PAGE                                
         GOTO1 HIGH                                                             
         MVI   COPYFLG,1 FIRST TIME                                             
         B     REPP1                                                            
***** PRINT CODE ENDS HERE                                                      
REP8A    GOTO1 STOPPRNT                                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    EXIT                                                             
         MVC   REPE1(7),=C'COMMAND'                                             
         CLI   REPDAP,C'Y'                                                      
         BNE   EXIT                                                             
*                                        DELETE MESSAGES JUST PRINTED           
         XC    KEY,KEY                                                          
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,MSGKTYPQ                                                
         MVC   MSGKFILE,OPNFILE                                                 
*                                                                               
REPDLP   GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(21),KEYSAVE                                                  
         BNE   REP9                                                             
*                                                                               
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OI    38(R6),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   SGNMSG(24),=C'MESSAGES DELETED--REPORT'                          
         B     REPDLP                                                           
*                                                                               
REP9     B     EXIT                                                             
         EJECT                                                                  
*                     PRINT HEADING                                             
         SPACE 3                                                                
HOOK     NTR1                                                                   
         LA    R4,KEY                                                           
         USING MSGKEYD,R4                                                       
         MVC   KEYSAVE,KEY                                                      
         L     R1,ABOX             FILL BOXES                                   
         LTR   R1,R1                                                            
         BZ    HOOK2                                                            
         USING BOXD,R1                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+82,C'R'                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         DROP  R1                                                               
         SPACE 2                                                                
HOOK2    MVC   H2+05(5),=C'FILE='                                               
         MVC   H2+10(8),MSGKFILE                                                
*                                                                               
         MVC   H2+24(20),SPACES                                                 
*                                  FROM                                         
         CLC   MSGKFROM,MSGNAM                                                  
         BE    OTFA1                                                            
         MVI   H2+24,C'F'                                                       
         MVC   H2+24+2(8),MSGKFROM                                              
*                                  AT                                           
         CLC   MSGKFRAT,TWAORIG                                                 
         BE    OTFA                                                             
         MVC   WORK+1(2),MSGKFRAT                                               
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVC   H2+24+11(8),WORK                                                 
*                           CHECK IF SENT                                       
OTFA1    MVI   ELCODE,X'33'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   OTFA                                                             
         MVI   H2+24,C'T'                                                       
         MVC   H2+24+2(8),4(R6)     FROM                                        
         MVC   H2+24+11(8),12(R6) TO                                            
OTFA     DS    0H                                                               
*                                  ADJUST DATE AND TIME                         
         MVC   WORK(1),SGNONOO                                                  
         CLC   OPNFILE,=C'INBOX   '                                             
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                  DATE                                         
         MVC   H2+44(5),=C'DATE='                                               
         GOTO1 DATCON,DMCB,(2,MSGKDATE),(5,WORK)                                
         MVC   H2+49(8),WORK                                                    
*                                  TIME                                         
         MVC   H2+59(5),=C'TIME='                                               
         SR    R0,R0                                                            
         ST    R0,WORK                                                          
         MVC   WORK+2(2),MSGKTIME                                               
         L     R1,WORK                                                          
         D     R0,=F'30'                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         LA    R1,8(R1)                                                         
         CVD   R1,WORK       HOURS                                              
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   H2+64(2),WORK                                                    
         MVI   H2+66,C':'                                                       
         CVD   R0,WORK       MINUTES                                            
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   H2+67(2),WORK                                                    
         MVC   KEY,KEYSAVE                                                      
*                                  SUBJECT                                      
         CLI   COPYFLG,1                                                        
         BNE   XIT                                                              
         MVI   COPYFLG,0                                                        
         MVC   H3+10(8),=C'SUBJECT='                                            
         MVI   ELCODE,X'30'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   H3+18(40),SPACES                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXMVC R1,H3+18,4(R6)                                                   
         B     XIT                                                              
         SPACE 1                                                                
HEDSPECS SSPEC H1,2,C'MESSAGE LIST'                                             
         DC    X'00'                                                            
         EJECT                                                                  
*              MISC                                                             
         SPACE 3                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
AENDSYSD DS    A                                                                
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDACTIVD                                                       
         SPACE 1                                                                
       ++INCLUDE DDSECURED                                                      
         SPACE 1                                                                
       ++INCLUDE DDSPOOK                                                        
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DMREQHDR                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMSGFILE                                                      
       ++INCLUDE PEMSGFFD                                                       
         PRINT ON                                                               
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGFBD                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080PEMSG04   05/01/02'                                      
         END                                                                    

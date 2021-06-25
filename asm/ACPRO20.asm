*          DATA SET ACPRO20    AT LEVEL 003 AS OF 09/12/02                      
*PHASE T60B20A,*                                                                
         TITLE 'T60B20 - SCHEME REPORT'                                         
T60B20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T60B20*,RA,CLEAR=YES                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         EJECT                                                                  
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
XIT      XIT1                                                                   
         EJECT                                                                  
VREC     NTR1                                                                   
         LA    R2,RPTSCHH          SCHEME IS OPTIONAL                           
         MVC   REQSCH,SPACES                                                    
         MVC   SCHEME,=CL8'A'      ASSUME ALL AND SET COMPARE LENGTH            
         MVC   EXLEN,=YL1(ACSHCODE-ACSHKEY)                                     
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    VREC040                                                          
         CLC   8(3,R2),=CL9'ALL'                                                
         BE    VREC040                                                          
         CLI   8(R2),C'*'          EXCLUDE THIS SCHEME ?                        
         BNE   VREC020             NO,INCLUDE                                   
         BCTR  R1,0                MINUS 1 FOR *                                
         BCTR  R1,0                MINUS 1 FOR EXECUTE                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REQSCH(0),9(R2)     SAVE SCHEME                                  
         NI    REQSCH,X'FF'-X'40'  TURN OFF 40 BIT                              
         B     VREC040                                                          
*                                                                               
VREC020  GOTO1 VALSCH                                                           
         MVC   REQSCH,SCHEME                                                    
         MVC   EXLEN,=YL1(ACSHCODE+8-ACSHKEY)                                   
*                                                                               
VREC040  LA    R2,RPTCATH          CATEGORY DETAIL OPTIONAL                     
         MVC   REQCAT,=C'N'        ASSUME NO                                    
         CLI   5(R2),0                                                          
         BE    VREC060                                                          
         CLI   8(R2),C'N'                                                       
         BE    VREC060                                                          
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVC   REQCAT,8(R2)                                                     
*                                                                               
VREC060  LA    R2,RPTWRKH          WORKCODE DETAIL OPTIONAL                     
         MVC   REQWRK,=C'N'        ASSUME NO                                    
         CLI   5(R2),0                                                          
         BE    VREC080                                                          
         CLI   8(R2),C'N'                                                       
         BE    VREC080                                                          
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVC   REQWRK,8(R2)                                                     
*                                                                               
VREC080  LA    R2,RPTINSH          INSTRUCTION DETAIL OPTIONAL                  
         MVC   REQINS,=C'N'        ASSUME NO                                    
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         CLI   8(R2),C'N'                                                       
         BE    VRECX                                                            
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVI   ERROR,NOTCOM                                                     
         CLI   RPTCATH+8,C'Y'                                                   
         BNE   ERREND                                                           
         MVC   REQINS,8(R2)                                                     
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
PREP     NTR1                                                                   
         LA    R6,KEY              READ SCHEME RECORD                           
         USING ACSHKEY,R6                                                       
         XC    ACSHKEY,ACSHKEY                                                  
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,SCHEME                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R2,HOOK             PREPARE BOXES                                
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         MVI   RCSUBPRG,0          SETUP FOR SCHEME ONLY                        
         MVC   USECOLS,TYP0COLS                                                 
         CLI   REQWRK,C'Y'         PRINTING WORKCODE DETAILS ?                  
         BNE   PREP020             NO                                           
         MVI   RCSUBPRG,2          YES,WORKCODE AND SCHEME                      
         MVC   USECOLS,TYP2COLS                                                 
         CLI   REQCAT,C'Y'         PRINTING CATEGORIES ?                        
         BNE   PREP040             NO, START READ                               
         MVI   RCSUBPRG,3          YES, SCHEME, CAT AND W/C                     
         MVC   USECOLS,TYP3COLS                                                 
         CLI   REQINS,C'Y'         PRINTING INSTRUCTIONS ?                      
         BNE   PREP040             NO, START READ                               
         MVI   RCSUBPRG,5          YES, SCHEME, CAT, INS  AND W/C               
         MVC   USECOLS,TYP5COLS                                                 
         B     PREP040                                                          
*                                                                               
PREP020  CLI   REQCAT,C'Y'         CATEGORY AND NO W/C ?                        
         BNE   PREP040             NO                                           
         MVI   RCSUBPRG,1          YES, SCHEME AND CATEGORY                     
         MVC   USECOLS,TYP1COLS                                                 
         CLI   REQINS,C'Y'         INSTRUCTIONS TOO ?                           
         BNE   PREP040             NO                                           
         MVI   RCSUBPRG,4          YES, SCHEME, CAT AND INSTRUCTIONS            
         MVC   USECOLS,TYP4COLS                                                 
*                                                                               
PREP040  BAS   RE,READHI                                                        
*                                                                               
PREP060  CLC   KEY(ACSHCODE-ACSHKEY),SAVEKEY                                    
         BNE   XIT                 DONE                                         
         CLC   REQSCH,SPACES       WAS A SCHEME ASKED FOR ?                     
         BE    PREP100             NO, CONTINUE                                 
         MVC   WORK(L'REQSCH),REQSCH                                            
         OI    WORK,X'40'                                                       
         TM    REQSCH,X'40'        IS THIS A POSITIVE FILTER ?                  
         BO    PREP080             YES                                          
         CLC   ACSHCODE,WORK       NO, USE NEGATIVE LOGIC                       
         BNE   PREP100             TAKES IF NOT EQUAL                           
         B     PREP180             SKIP IF EQUAL                                
*                                                                               
PREP080  CLC   ACSHCODE,WORK       POSITIVE LOGIC                               
         BNE   PREP180             TAKE IF EQUAL                                
*                                                                               
PREP100  MVC   SAVEKEY,KEY         SAVE KEY FOR RE-READ                         
*                                                                               
         BAS   RE,PRTSCH           PRINT SCHEME NAME                            
*                                                                               
         CLI   REQCAT,C'Y'         PRINTING CATEGORY DETAIL ?                   
         BE    PREP120             YES                                          
         CLI   REQWRK,C'Y'         NO, PRINTING WORKCODE DETAIL ?               
         BNE   PREP180             NO, GET NEXT SCHEME RECORD                   
*                                                                               
PREP120  MVI   ELCODE,ACSCELQ      READ SEQUENCE ELEMENT FOR CATEGORIES         
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ELEMENT                            
         USING ACSCD,R6                                                         
         SR    R1,R1                                                            
         IC    R1,ACSCLEN                                                       
         SH    R1,=H'2'                                                         
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE AT LEAST "SLUSH"                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SEQLIST(0),ACSCAT                                                
         LA    R4,SEQLIST(R1)                                                   
         MVI   1(R4),X'FF'         SET END MARKER                               
         LA    R4,SEQLIST          ADDRESS LIST                                 
*                                                                               
PREP140  CLC   0(4,R4),=X'FFFF'    SLUSH ACCOUNT ?                              
         BE    PREP160             YES, SKIP IT                                 
         BAS   RE,READCAT          NO, READ IT                                  
         BNE   PREP160                                                          
*                                                                               
         CLI   REQCAT,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,PRTCAT                                                        
         CLI   REQWRK,C'Y'         PRINTING WORKCODE ALSO ?                     
         BNE   *+8                                                              
         BAS   RE,PRTWRK                                                        
*                                                                               
PREP160  LA    R4,L'SEQLIST(R4)    GET NEXT CATEGORY                            
         CLI   0(R4),X'FF'         IF ANY LEFT                                  
         BNE   PREP140                                                          
*                                                                               
         MVC   P,SPACES                                                         
         BAS   RE,PRINT                                                         
         MVC   KEY,SAVEKEY         RESTORE SCHEME KEY                           
         BAS   RE,REREAD           RE-READ THE RECORD                           
*                                                                               
PREP180  BAS   RE,READNXT          DO SEQUENTIAL READ                           
         B     PREP060                                                          
         EJECT                                                                  
PRTSCH   ST    RE,SAVERE           PRINT DATA FOR SCHEME(S)                     
         LA    R3,P                                                             
         USING SCHHEAD,R3                                                       
         USING ACSHKEY,R6                                                       
         MVC   SCHCODE,ACSHCODE    SCHEME CODE                                  
*                                                                               
         MVI   ELCODE,ACSDELQ      GET DEFINITION ELEMENT                       
         BAS   RE,GETELIO                                                       
         BNE   PRTS020                                                          
         USING ACSDD,R6                                                         
         MVC   SCHNAME,ACSDNAME    SCHEME NAME                                  
*                                                                               
PRTS020  BAS   RE,PRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
READCAT  ST    RE,SAVERE           READ CATEGORY RECORD                         
         LA    R6,KEY                                                           
         USING ACCTKEY,R6                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         MVC   ACCTSCH,SAVEKEY+(ACSHCODE-ACSHKEY)                               
         MVC   ACCTCODE,0(R4)                                                   
         OI    DMINBTS,X'08'       LOOK FOR DELETED AS WELL                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(ACCTCODE-ACCTKEY+2),KEY                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
PRTCAT   ST    RE,SAVERE           PRINT DATA FOR CATEGORY(IES)                 
         LA    R3,P                                                             
         USING CATHEAD,R3                                                       
         USING ACCTKEY,R6                                                       
         MVC   CATCODE,ACCTCODE    CATEGORY CODE                                
*                                                                               
         MVI   ELCODE,ACCDELQ      DESCRIPTION ELEMENT                          
         BAS   RE,GETELIO                                                       
         BNE   PRTC040                                                          
         USING ACCDD,R6                                                         
         MVC   CATNAME,ACCDNAME    CATEGORY NAME                                
         CLI   REQINS,C'Y'         PRINT INSTRUCTIONS ?                         
         BNE   PRTC040             NO                                           
*                                                                               
         MVC   CATINST,SPACES      YES, CLEAR THE FIELD                         
         CLC   ACCDLEN,=YL1(ACCDINST-ACCDD) ANYTHING THERE ?                    
         BE    PRTC020             NO                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,ACCDLINP                                                    
         BZ    PRTC040             NOTHING THERE                                
         BCTR  R1,0                MINUS 1 FOR LENGTH                           
         BCTR  R1,0                MINUS 1 FOR MOVE                             
         EX    R1,*+8                                                           
         B     PRTC040                                                          
         MVC   CATINST(0),ACCDINP                                               
*                                                                               
PRTC020  TM    ACCDTYPE,X'02'      IS THIS AN AGENCY ?                          
         BZ    PRTC040             NO                                           
         MVC   CATINST(3),=C'AGY'  YES                                          
*                                                                               
PRTC040  BAS   RE,PRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
PRTWRK   ST    RE,SAVERE           PRINT DATA FOR WORKCODE(S)                   
         LA    R3,P                                                             
         CLI   REQCAT,C'Y'         ALSO PRINTING CATEGORY ?                     
         BNE   PRTW020             NO                                           
         LA    R3,29(R3)           YES, SKIP PAST CATEGORY                      
         CLI   REQINS,C'Y'         ALSO INSTRUCTIONS ?                          
         BNE   *+8                 NO                                           
         LA    R3,20(R3)           YES, GET PAST INSTRUCTIONS                   
         USING WRKHEAD,R3                                                       
PRTW020  MVI   ELCODE,ACCWELQ      WORK CODE ELEMENT                            
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
PRTW040  BAS   RE,NEXTEL                                                        
         BNE   PRTW100                                                          
         USING ACCWD,R6                                                         
         CLI   ACCWTYPE,1          IS THIS A WORKCODE ?                         
         BNE   PRTW040             NO, READ AGAIN                               
         MVC   WRKCODE,ACCWWORK    WORK CODE                                    
         CLI   ACCWLEN,ACCWLNQ1    ANY DESCRIPTION ?                            
         BE    PRTW060             NO, GET NEXT                                 
         MVC   WRKOVER,ACCWDESC    AND DESCRIPTION                              
*                                                                               
PRTW060  MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),ACCWWORK                                                
         MVC   AIO,AIO2                                                         
         OI    DMINBTS,X'08'       LOOK FOR DELETED AS WELL                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACKEYACC),KEY                                          
         BNE   PRTW080                                                          
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETELIO2                                                      
         BNE   PRTW080                                                          
         USING ACANALD,R5                                                       
         MVC   WRKDESC,ACANDESC                                                 
*                                                                               
PRTW080  MVC   AIO,AIO1                                                         
         BAS   RE,PRINT                                                         
         MVI   ELCODE,ACCWELQ                                                   
         B     PRTW040                                                          
*                                                                               
PRTW100  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
READHI   ST    RE,SAVERE                                                        
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 HIGH                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
READNXT  ST    RE,SAVERE                                                        
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 SEQ                                                              
         L     R6,AIO                                                           
         MVC   KEY,0(R6)           UPDATE KEY                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
REREAD   ST    RE,SAVERE                                                        
         OI    DMINBTS,X'08'                                                    
         GOTO1 READ                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                                                                   
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXREQ,C' '                                                      
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLS,USECOLS                                                  
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
MYSPECS  DS    0F                                                               
         SPROG 0,1,2,3,4,5                                                      
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         SSPEC H1,48,C'SCHEME REPORT'                                           
         SSPEC H2,48,C'_____________'                                           
         SSPEC H7,2,C'SCHEME'                                                   
         SSPEC H7,14,C'SCHEME NAME'                                             
         SPROG 1                                                                
         SSPEC H7,37,C'CAT'                                                     
         SSPEC H7,43,C'CATEGORY DESCRIPTION'                                    
         SPROG 2                                                                
         SSPEC H7,37,C'W/C'                                                     
         SSPEC H7,43,C'W/C DESCRIPTION'                                         
         SSPEC H7,61,C'W/C OVERRIDE'                                            
         SPROG 3                                                                
         SSPEC H7,37,C'CAT'                                                     
         SSPEC H7,43,C'CATEGORY DESCRIPTION'                                    
         SSPEC H7,66,C'W/C'                                                     
         SSPEC H7,72,C'W/C DESCRIPTION'                                         
         SSPEC H7,90,C'W/C OVERRIDE'                                            
         SPROG 4                                                                
         SSPEC H7,37,C'CAT'                                                     
         SSPEC H7,43,C'CATEGORY DESCRIPTION'                                    
         SSPEC H7,66,C'INSTRUCTIONS'                                            
         SPROG 5                                                                
         SSPEC H7,37,C'CAT'                                                     
         SSPEC H7,43,C'CATEGORY DESCRIPTION'                                    
         SSPEC H7,66,C'INSTRUCTIONS'                                            
         SSPEC H7,86,C'W/C'                                                     
         SSPEC H7,92,C'W/C DESCRIPTION'                                         
         SSPEC H7,110,C'W/C OVERRIDE'                                           
         DC    X'00'                                                            
         EJECT                                                                  
TYP5COLS DS    0CL132                                                           
         DC    C'L'                +0                                           
         DS    CL10                +1                                           
         DC    C'C'                +11                                          
         DS    CL22                +12                                          
         DC    C'C'                +34                                          
         DS    CL5                 +35                                          
         DC    C'C'                +40                                          
         DS    CL22                +41                                          
         DC    C'C'                +63                                          
         DS    CL19                +64                                          
         DC    C'C'                +83                                          
         DS    CL5                 +84                                          
         DC    C'C'                +89                                          
         DS    CL17                +90                                          
         DC    C'C'                +107                                         
         DS    CL17                +108                                         
         DC    C'R'                +125                                         
         DS    CL7                                                              
         SPACE 3                                                                
TYP4COLS DS    0CL132                                                           
         DC    C'L'                +0                                           
         DS    CL10                +1                                           
         DC    C'C'                +11                                          
         DS    CL22                +12                                          
         DC    C'C'                +34                                          
         DS    CL5                 +35                                          
         DC    C'C'                +40                                          
         DS    CL22                +41                                          
         DC    C'C'                +63                                          
         DS    CL17                +64                                          
         DC    C'R'                +81                                          
         DS    CL51                                                             
         SPACE 3                                                                
TYP3COLS DS    0CL132                                                           
         DC    C'L'                +0                                           
         DS    CL10                +1                                           
         DC    C'C'                +11                                          
         DS    CL22                +12                                          
         DC    C'C'                +34                                          
         DS    CL5                 +35                                          
         DC    C'C'                +40                                          
         DS    CL22                +41                                          
         DC    C'C'                +63                                          
         DS    CL5                 +64                                          
         DC    C'C'                +69                                          
         DS    CL17                +70                                          
         DC    C'C'                +87                                          
         DS    CL18                +88                                          
         DC    C'R'                +106                                         
         DS    CL25                                                             
         SPACE 3                                                                
TYP2COLS DS    0CL132                                                           
         DC    C'L'                +0                                           
         DS    CL10                +1                                           
         DC    C'C'                +11                                          
         DS    CL22                +12                                          
         DC    C'C'                +34                                          
         DS    CL5                 +35                                          
         DC    C'C'                +40                                          
         DS    CL17                +41                                          
         DC    C'C'                +58                                          
         DS    CL17                +59                                          
         DC    C'R'                +76                                          
         DS    CL55                +77                                          
         SPACE 3                                                                
TYP1COLS DS    0CL132                                                           
         DC    C'L'                +0                                           
         DS    CL10                +1                                           
         DC    C'C'                +11                                          
         DS    CL22                +12                                          
         DC    C'C'                +34                                          
         DS    CL5                 +35                                          
         DC    C'C'                +40                                          
         DS    CL22                +41                                          
         DC    C'R'                +63                                          
         DS    CL67                +64                                          
         SPACE 3                                                                
TYP0COLS DS    0CL132                                                           
         DC    C'L'                +0                                           
         DS    CL10                +1                                           
         DC    C'C'                +11                                          
         DS    CL22                +12                                          
         DC    C'R'                +34                                          
         DS    CL96                +35                                          
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
GETELIO2 L     R5,AIO                                                           
         GETEL2 (R5),DATADISP,ELCODE                                            
         SPACE 3                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 3                                                                
DUMP     DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SCHHEAD  DSECT                                                                  
         DS    CL2                                                              
SCHCODE  DS    CL8                                                              
         DS    CL3                                                              
SCHNAME  DS    CL20                                                             
         EJECT                                                                  
CATHEAD  DSECT                                                                  
         DS    CL36                                                             
CATCODE  DS    CL2                                                              
         DS    CL4                                                              
CATNAME  DS    CL20                                                             
         DS    CL3                                                              
CATINST  DS    CL17                                                             
         EJECT                                                                  
WRKHEAD  DSECT                                                                  
         DS    CL36                                                             
WRKCODE  DS    CL3                                                              
         DS    CL3                                                              
WRKDESC  DS    CL15                                                             
         DS    CL3                                                              
WRKOVER  DS    CL15                                                             
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROD0D                                                       
SEQLIST  DS    0XL2                                                             
         DS    XL(2*126)                                                        
SEQEND   DS    XL1                                                              
*                                                                               
USECOLS  DS    CL132               HOLD AREA FOR COLUMNS IN USE                 
SAVEKEY  DS    CL42                                                             
SAVERE   DS    A                                                                
EXLEN    DS    XL1                                                              
REQSCH   DS    XL8                                                              
REQCAT   DS    XL1                                                              
REQWRK   DS    XL1                                                              
REQINS   DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACPRO20   09/12/02'                                      
         END                                                                    

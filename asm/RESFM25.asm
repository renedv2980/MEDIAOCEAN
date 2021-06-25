*          DATA SET RESFM25    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T81825A                                                                  
         TITLE 'T81825 - RESFM25 - DIRECT RESPONSE REPORT'                      
***********************************************************************         
*                                                                     *         
*  RESFM25 (T81825) --- REPORT OF DIRECT RESPONSE CARDS               *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 01SEP92 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 28FEB94 (SKU) FILTER ON PERIOD BEFORE GROUP (IF ANY) INTERNALLY SO  *         
*               THE NUMBER OF IO'S ARE REDUCED                        *         
*                                                                     *         
* 09OCT96 (SEP) ALLOW LOW POWER TV STATIONS                                     
*                                                                     *         
* 19NOV97 (JRD) YR2000 PWOS DATE                                      *         
***********************************************************************         
T81825   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81825*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
K        USING RDIRKEY,KEY         REMOVE HARD KEY LENGTHS                      
KS       USING RDIRKEY,KEYSAVE     REMOVE HARD KEY LENGTHS                      
SV       USING RDIRKEY,SVKEY       REMOVE HARD KEY LENGTHS                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         CLC   =C'..',DRRGRUP      FLAG TO TELL US THAT WE CAME BY A            
         BNE   VK03                PFKEY.  CLEAR GROUP AND PROMPT USER          
         XC    DRRGRUP,DRRGRUP     FOR INPUT.                                   
         MVI   DRRGRUPH+5,0                                                     
         MVI   DRRGRUPH+4,0        CLEAR INPUT FLAG                             
         OI    DRRGRUPH+6,X'80'+X'01' XMIT/MODIFIED IN CASE THE USER            
         LA    R2,DRRGRUPH         IS HAPPY WITH THE FILTERS ALREADY            
         B     GETFILT             PLEASE ENTER FILTER                          
*                                                                               
* EITHER GROUP/SUBGROUP OR STATION CAN BE ENTERED                               
*                                                                               
VK03     DS    0H                                                               
         CLI   DRRGRUPH+5,0        GROUP/SUB-GROUP?                             
         BE    VK08                                                             
*                                                                               
         LA    R2,DRRSTATH                                                      
         CLI   DRRSTATH+5,0                                                     
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,DRRGRUPH                                                      
         OC    DRRGRUP,SPACES      BLANK PAD                                    
*                                                                               
         XC    KEY,KEY             GET GROUP/SUBGROUP RECORD                    
         LA    R6,KEY                                                           
         USING RGRPKEY,R6                                                       
         MVI   RGRPKTYP,X'07'                                                   
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,DRRGRUP                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RGRPKEY),KEYSAVE                                           
         BNE   INVLGRP                                                          
         B     VK20                                                             
*                                                                               
VK08     DS    0H                                                               
         LA    R2,DRRSTATH                                                      
         CLI   DRRSTATH+5,0        STATION?                                     
         BE    MISSFLD                                                          
*                                                                               
* VALIDATE STATION CALL LETTERS                                                 
*                                                                               
VK10     DS    0H                                                               
         CLI   DRRSTATH+5,0        STATION?                                     
         BE    VK20                                                             
         LA    R2,DRRSTATH         VALIDATE STATION CALL LETTER FORMAT          
         GOTO1 VALISTA                                                          
         MVC   STATION,WORK        SAVE CALL LETTERS                            
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VK20     DS    0H                                                               
         CLI   DRRPERIH+5,0        PERIOD?                                      
         BE    VK30                                                             
*                                                                               
         LA    R2,DRRPERIH         VALIDATE PERIOD                              
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    INVLFLD             ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R5,BLOCK                                                         
         GOTO1 DATVAL,DMCB,12(R5),WORK                                          
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK,(19,STARTDT)     START DATE                     
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         GOTO1 DATVAL,DMCB,22(R5),WORK+6                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK+6,(19,ENDDT)     END DATE                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),ENDDT(3)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   ENDDT,WORK                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),STARTDT(3) CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   STARTDT,WORK                                                     
*                                                                               
VK30     DS    0H                                                               
         CLI   DRRDATEH+5,0        ACTIVITY DATE                                
         BE    VKX                                                              
*                                                                               
         GOTO1 DATVAL,DMCB,DRRDATE,WORK                                         
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLFLD                                                          
         GOTO1 DATCON,DMCB,WORK,(3,ACTVDT)      ACTIVITY DATE                   
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS REPORT                                                                
***********************************************************************         
PR       DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
*                                                                               
         CLI   DRRSTATH+5,0        FILTER ON STATION?                           
         BE    PR10                                                             
         MVC   RDIRSTA,STATION     YES, START WITH THIS STATION                 
         DROP  R6                                                               
*                                                                               
PR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
PR20     DS    0H                                                               
         CLC   KEY(RDIRSTA-RDIRKEY),KEYSAVE                                     
         BNE   PRX                                                              
*                                                                               
         CLI   DRRSTATH+5,0        FILTER ON STATION                            
         BE    PR30                                                             
         CLC   K.RDIRSTA,KS.RDIRSTA                                             
         BNE   PRX                 WANT ONLY RECORDS WITH THIS STATION          
*                                                                               
PR30     CLI   K.RDIRMAST,0        MUST BE MASTER RECORD                        
         BNE   PRSEQ                                                            
*                                                                               
         CLI   DRRPERIH+5,0        FILTER ON PERIOD?                            
         BE    PR35                                                             
*                                                                               
* PERIOD FILTER MUST AT LEAST OVERLAP                                           
*                                                                               
         CLC   STARTDT,K.RDIRENDT  THESE DATES ARE IN 9'S COMP!                 
         BL    PRSEQ                                                            
         CLC   ENDDT,K.RDIRSTDT                                                 
         BH    PRSEQ                                                            
*                                                                               
PR35     DS    0H                                                               
         CLI   DRRGRUPH+5,0        FILTER ON GROUP?                             
         BE    PR60                                                             
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),SV.RDIRSTA                                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     STATION RECORD MUST BE THERE                 
         BE    PR40                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RESTORE SEQ ORDER                            
         B     PRSEQ                                                            
*                                                                               
PR40     MVC   AIO,AIO2            USE IO2                                      
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R1,DRRGRUPH+5       MATCH ON GROUP/SUBGROUP                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   RSTAGRUP(0),DRRGRUP                                              
         BE    PR50                                                             
         DROP  R6                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ ORDER                            
         B     PRSEQ                                                            
*                                                                               
PR50     DS    0H                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                RESTORE SEQ ORDER                            
*                                                                               
PR60     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   DRRDATEH+5,0        FILTER ON ACTIVITY DATE?                     
         BE    PR70                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDIRDESD,R6                                                      
         MVI   ELCODE,RDIRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   ACTVDT,RDIRDLUP     STARTS FROM THIS ACTIVITY DATE               
         BH    PRSEQ                                                            
         DROP  R6                                                               
*                                                                               
PR70     DS    0H                                                               
         L     R6,AIO              SAVE OFF CONTROL ID                          
         USING RDIRREC,R6                                                       
         MVC   CTRLID,RDIRDCID                                                  
         MVC   LASTUPD,RDIRDLUP                                                 
         MVC   STATION,RDIRSTA     SAVE OFF FOR HEADHOOK                        
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RDIRPRGD,R6                                                      
         MVI   ELCODE,RDIRPCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PRSEQ                                                            
*                                                                               
* CONSTRUCT ONE REPORT RECORD                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     PR90                                                             
*                                                                               
PR80     DS    0H                                                               
         OC    RDIRPDAY,RDIRPDAY                                                
         BZ    PR90                                                             
         BAS   RE,PRINT                                                         
*                                                                               
PR90     MVC   PDAY,RDIRPDAY                                                    
         MVC   PTIME,RDIRPTIM                                                   
         MVC   PPROGRAM,RDIRPROG                                                
         MVC   P30,RDIRP30                                                      
         MVC   P60,RDIRP60                                                      
         MVC   P90,RDIRP90                                                      
         MVC   P120,RDIRP120                                                    
         BAS   RE,PRINT                                                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    PR80                                                             
         DROP  R6                                                               
*                                                                               
         MVC   SVKEY,KEY           SAVE OFF KEY TO READ NOTES                   
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVI   RDIRMAST,1                                                       
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   PR110                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDIRNTED,R6                                                      
         MVI   ELCODE,RDIRNCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR110                                                            
*                                                                               
         BAS   RE,PRINT                                                         
         MVC   P+14(6),=C'NOTES:'                                               
         BAS   RE,PRINT                                                         
         MVI   SEQNUM,0                                                         
         B     PR103                                                            
*                                                                               
PR100    DS    0H                                                               
         BAS   RE,PRINT                                                         
*                                                                               
PR103    DS    0H                                                               
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         CLC   SEQNUM,RDIRNTSQ                                                  
         BL    PR100                                                            
*                                                                               
         CLC   =C'C=',RDIRNOTE     CHECK FOR STORED COMMENTS                    
         BE    PR105                                                            
         ZIC   R1,RDIRNELN         PRINT THE NOTES                              
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+14(0),RDIRNOTE                                                 
         BAS   RE,PRINT                                                         
         B     PR108                                                            
*                                                                               
PR105    DS    0H                  EXPAND THE STORED COMMENTS                   
         GOTO1 PRTSTCMT,DMCB,RDIRNOTE                                           
*                                                                               
PR108    BAS   RE,NEXTEL                                                        
         BE    PR103                                                            
*                                                                               
PR110    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
*                                                                               
PRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     PR20                                                             
*                                                                               
PRX      B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT STORED COMMENTS                                                         
* USES IO3                                                                      
***********************************************************************         
PRTSTCMT NTR1                                                                   
         MVC   SVELCODE,ELCODE     SAVE OFF ELCODE                              
         MVC   ANOTE,0(R1)          A(RDIRNOTE)                                 
         MVC   SVKEY2,KEY                                                       
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,AGENCY     REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         L     R1,ANOTE                                                         
         MVC   RCMTKCDE,2(R1)      COMMENT CODE                                 
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    PRTST10                                                          
*                                                                               
         L     R1,ANOTE                                                         
         MVC   P+14(10),0(R1)      DIDN'T FIND THE CODE                         
         BAS   RE,PRINT            PRINT THE CODE AND EXIT                      
         B     PRTSTX                                                           
*                                                                               
PRTST10  DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCMTELM2,R6                                                      
         MVI   ELCODE,2            COMMENT TEXT ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PRTSTX              NO TEXT FOUND                                
*                                                                               
PRTST20  DS    0H                                                               
         CLI   RCMT2LEN,3          GET NON-BLANK COMMT LINE                     
         BH    PRTST40                                                          
         CLI   RCMT2TXT,C' '                                                    
         BNE   PRTST40                                                          
*                                                                               
PRTST30  BAS   RE,NEXTEL                                                        
         BE    PRTST20                                                          
         B     PRTSTX                                                           
*                                                                               
PRTST40  DS    0H                                                               
         ZIC   R1,RCMT2LEN         MOVE IN LENGTH OF COMMENT                    
         SH    R1,=H'2'            SUBTRACT CODE AND LENGTH                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+14(0),RCMT2TXT    MOVE IN COMMENT                              
         BAS   RE,PRINT                                                         
         B     PRTST30                                                          
*                                                                               
PRTSTX   DS    0H                                                               
         MVC   KEY,SVKEY2                                                       
         MVC   AIO,AIO1                                                         
         MVC   ELCODE,SVELCODE                                                  
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
GETFILT  MVC   RERROR,=AL2(ASKFILT)                                             
         B     ERREND                                                           
*                                                                               
INVLGRP  MVC   RERROR,=AL2(INVGRP)                                              
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,1,C'CONTROL ID:'                                              
         PSPEC H2,76,REQUESTOR                                                  
         PSPEC H9,1,C'DAY'                                                      
         PSPEC H9,15,C'TIME'                                                    
         PSPEC H9,34,C'COMMENTS/PROGRAM'                                        
         PSPEC H9,66,C':30   :60   :90   :120'                                  
         PSPEC H10,1,9C'-'                                                      
         PSPEC H10,15,14C'-'                                                    
         PSPEC H10,34,27C'-'                                                    
         PSPEC H10,66,C'----  ----  ----  ----'                                 
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
         MVC   H2+13(L'CTRLID),CTRLID                                           
         MVI   H2+21,C'/'                                                       
         GOTO1 DATCON,DMCB,(3,LASTUPD),(0,H2+22)                                
         MVC   H5(4),STATION                                                    
         MVC   H5+4(5),=C'- M /'                                                
         MVC   H5+5(1),STATION+4                                                
         CLI   H5+5,C'L'                                                        
         BE    HOOK05                                                           
         CLI   H5+5,C' '                                                        
         BNE   HOOK10                                                           
         MVC   H5+5(2),=C'TV'     TV                                            
         B     HOOK10                                                           
HOOK05   EQU   *                                                                
         MVC   H5+4(5),=C'-   /'                                                
         MVC   H5+5(1),STATION+4                                                
*                                  PERIOD                                       
HOOK10   DS    0H                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRSTDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,H7+15)                                   
         MVI   H7+23,C'-'                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),RDIRENDT(3)  CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,H7+24)                                   
         DROP  R6                                                               
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,SV.RDIRSTA                                              
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   HOOK20                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
         CLC   RSTAAFFL,SPACES                                                  
         BNE   HOOK15                                                           
         MVC   H5+10(L'RSTAMKT),RSTAMKT                                         
         B     HOOK20                                                           
*                                                                               
HOOK15   MVC   H5+10(5),=C'(   )'                                               
         MVC   H5+11(L'RSTAAFFL),RSTAAFFL                                       
         MVC   H5+16(L'RSTAMKT),RSTAMKT                                         
         OC    H5+16(L'RSTAMKT),SPACES    BLANK PAD FOR CENTERING               
         DROP  R6                                                               
*                                                                               
         GOTO1 CENTER,DMCB,H5,88                                                
         MVC   H6(25),=C'DIRECT RESPONSE RATE CARD'                             
         GOTO1 CENTER,DMCB,H6,88                                                
         MVC   H7(15),=C'FOR THE PERIOD '                                       
         GOTO1 CENTER,DMCB,H7,88                                                
*                                                                               
HOOK20   DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RE-ESTABLISH SEQ ORDER                       
*                                                                               
HOOKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCAL STORAGE AREA                                                            
***********************************************************************         
STATION  DS    CL5                                                              
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
ACTVDT   DS    XL3                                                              
SVKEY    DS    CL(L'KEY)                                                        
SVKEY2   DS    CL(L'KEY)                                                        
CTRLID   DS    CL(L'RDIRDCID)                                                   
LASTUPD  DS    CL(L'RDIRDLUP)                                                   
ANOTE    DS    A                   ADDRESS OF STORED COMMENT                    
SVELCODE DS    X                   SAVES ELCODE                                 
SEQNUM   DS    X                   SEQUENCE NUMBER                              
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE RESFMFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC8D          (OUR REPORT SCREEN OVERLAY)                  
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENDIR                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENCMT                                                       
         PRINT ON                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PDAY     DS    CL(L'RDIRPDAY)                                                   
         DS    CL5                                                              
PTIME    DS    CL(L'RDIRPTIM)                                                   
         DS    CL5                                                              
PPROGRAM DS    CL(L'RDIRPROG)                                                   
         DS    CL5                                                              
P30      DS    CL(L'RDIRP30)                                                    
         DS    CL2                                                              
P60      DS    CL(L'RDIRP60)                                                    
         DS    CL2                                                              
P90      DS    CL(L'RDIRP90)                                                    
         DS    CL2                                                              
P120     DS    CL(L'RDIRP120)                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012RESFM25   05/01/02'                                      
         END                                                                    

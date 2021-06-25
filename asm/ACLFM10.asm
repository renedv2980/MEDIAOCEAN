*          DATA SET ACLFM10    AT LEVEL 044 AS OF 05/01/02                      
*PHASE T60310A,*                                                                
*&&UK                                                                           
*INCLUDE ACBLTYP                                                                
*&&                                                                             
*&&US                                                                           
*INCLUDE ACDISOPT                                                               
*&&                                                                             
*INCLUDE SQUASHER                                                               
*INCLUDE BINSRCH                                                                
         TITLE 'ESTIMATING ON-LINE'                                             
T60310   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ESTIMX-ESTIMD,**L01B**,R9,RR=R5,CLEAR=YES                        
         LR    R8,RC                                                            
         USING ESTIMD,R8           PROGRAM WORKING STORAGE                      
         LA    R7,TWA1                                                          
         USING TWA1D,R7            SAVED TWA                                    
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)            LFM WORKING STORAGE                          
         USING LOGWORKD,RC                                                      
         ST    R5,RELO                                                          
*&&UK                                                                           
         L     R2,=V(ACBLTYP)                                                   
         A     R2,RELO                                                          
         ST    R2,ACBLTYP                                                       
*&&                                                                             
*&&US                                                                           
         L     R2,=V(ACDISOPT)                                                  
         A     R2,RELO                                                          
         ST    R2,DISOPT                                                        
*&&                                                                             
         L     R2,=V(SQUASHER)                                                  
         A     R2,RELO                                                          
         ST    R2,SQUASHER                                                      
         L     R2,=V(BINSRCH)                                                   
         A     R2,RELO                                                          
         ST    R2,BINSRCH                                                       
         L     R2,=A(JOBTOTS)                                                   
         A     R2,RELO                                                          
         ST    R2,AJOBTOTS                                                      
         SPACE 1                                                                
         MVC   DMCB+10(2),2(RA)    GET SAVED TWA1                               
         MVC   DMCB+8(2),=X'0100'                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,TWA1                        
         MVI   ERROR,OK                                                         
         ZAP   TOTINCR,=P'0'                                                    
         MVI   THISACT,NEW                                                      
         CLI   LOGACT,C'N'                                                      
         BE    *+8                                                              
         MVI   THISACT,AMEND                                                    
         CLC   THISACT,LASTACT                                                  
         BE    *+8                                                              
         NI    LOGCLIH+4,X'DF'     IF ACTION CHANGES - START AGAIN              
         MVC   LASTACT,THISACT                                                  
         OI    LOGSERVH+1,X'01'    SERVICE FIELD IS ALWAYS MODIFIED             
         OI    LOGSERVH+6,X'80'                                                 
         MVC   LOGHEAD,SPACES                                                   
         OI    LOGHEADH+6,X'80'                                                 
         EJECT                                                                  
*              ENSURE COMPANY/LEDGER DATA IS IN CORE                            
         SPACE 1                                                                
EM4      CLI   MODE,BUILDKEY                                                    
         BNE   EM40                                                             
         LA    R2,LOGCLIH                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 READ                                                             
         GOTO1 GETEL,DMCB,(X'10',IO),0                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING ACCOMPD,R4                                                       
         L     R4,DMCB+12                                                       
         MVC   SAVESTAT,ACMPSTAT   SAVE STATUS BYTES                            
         MVC   SAVESTA2,ACMPSTA2                                                
         MVC   36(2,RA),ACMPJOB                                                 
         MVC   KEY+1(2),ACMPJOB                                                 
         SPACE 1                                                                
         GOTO1 READ                                                             
         GOTO1 GETEL,DMCB,(X'16',IO),0                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R4                                                       
         L     R4,DMCB+12                                                       
         MVC   38(1,RA),ACHRLEVA                                                
         MVC   39(1,RA),ACHRLEVB                                                
         EJECT                                                                  
*              BUILD KEY (CLIENT ROUTINES)                                      
         SPACE 1                                                                
         OI    LOGHDLH+6,X'80'                                                  
*&&US                                                                           
         MVC   LOGHDL,=C'ORIGINAL                      CURRENT    '             
         TM    SAVESTA2,X'20'      USE BILLING AMOUNT ON HEADLINE               
         BZ    EM5                 NO                                           
         MVC   LOGHDL,=C'EXTERNAL                      INTERNAL   '             
*&&                                                                             
*&&UK                                                                           
         TM    SAVESTA2,X'20'      USE BILLING AMOUNT ON HEADLINE               
         BZ    EM5                 NO                                           
         MVC   LOGHDL,=C'EXTERNAL               INTERNAL          '             
         MVC   LOGNAR1+21(8),=C'EXTERNAL'                                       
         OI    LOGNAR1H+6,X'80'                                                 
         MVC   LOGNAR2(8),=C'INTERNAL'                                          
         OI    LOGNAR2H+6,X'80'                                                 
*&&                                                                             
         SPACE 1                                                                
EM5      TM    LOGCLIH+4,X'20'                                                  
         BO    EM8                                                              
         NI    LOGPRDH+4,X'DF'                                                  
         NI    LOGJOBH+4,X'DF'                                                  
         NI    LOGWCH+4,X'DF'                                                   
*&&US*&& NI    LOGREVCH+4,X'DF'                                                 
         MVC   LOGCLIN,SPACES                                                   
         OI    LOGCLINH+6,X'80'                                                 
         MVC   LOGPRDN,SPACES                                                   
         OI    LOGPRDNH+6,X'80'                                                 
         MVC   LOGJOBN,SPACES                                                   
         OI    LOGJOBNH+6,X'80'                                                 
*&&US*&& MVC   LOGREVN,SPACES                                                   
*&&US*&& OI    LOGREVNH+6,X'80'                                                 
*&&US*&& MVC   LOGREVD,SPACES                                                   
*&&US*&& OI    LOGREVDH+6,X'80'                                                 
         MVI   FIRST,0                                                          
         BAS   RE,CLRSCR                                                        
         SPACE 1                                                                
EM8      GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   5(1,R2),38(RA)      TEST IF CODE IS TOO LONG                     
         BH    INVINP                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),36(RA)                                                  
         MVC   KEY+3(6),WORK                                                    
         TM    LOGCLIH+4,X'20'                                                  
         BO    EM12                                                             
         SPACE 1                                                                
         GOTO1 READ                                                             
         XC    SAVECXPR,SAVECXPR                                                
         LA    R3,SAVECPRF                                                      
         LA    R5,SAVECXPR                                                      
         BAS   RE,ANYPROF                                                       
         GOTO1 NAMOUT                                                           
         OI    LOGCLIH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         EJECT                                                                  
*              BUILD KEY (PRODUCT ROUTINES)                                     
         SPACE 1                                                                
EM12     LA    R2,LOGPRDH                                                       
         TM    LOGPRDH+4,X'20'                                                  
         BO    EM14                                                             
         NI    LOGJOBH+4,X'DF'                                                  
         NI    LOGWCH+4,X'DF'                                                   
*&&US*&& NI    LOGREVCH+4,X'DF'                                                 
         MVC   LOGPRDN,SPACES                                                   
         OI    LOGPRDNH+6,X'80'                                                 
         MVC   LOGJOBN,SPACES                                                   
         OI    LOGJOBNH+6,X'80'                                                 
*&&US*&& MVC   LOGREVN,SPACES                                                   
*&&US*&& OI    LOGREVNH+6,X'80'                                                 
*&&US*&& MVC   LOGREVD,SPACES                                                   
*&&US*&& OI    LOGREVDH+6,X'80'                                                 
         MVI   FIRST,0                                                          
         BAS   RE,CLRSCR                                                        
         SPACE 1                                                                
EM14     GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         SR    R3,R3                                                            
         IC    R3,38(RA)                                                        
         LA    R5,KEY+3(R3)                                                     
         MVC   0(6,R5),WORK                                                     
         ZIC   R1,39(RA)                                                        
         SR    R1,R3               LENGTH OF PRODUCT CODE                       
         CLM   R1,1,5(R2)          TEST AGAINST INPUT LENGTH                    
         BL    INVINP                                                           
         TM    LOGPRDH+4,X'20'                                                  
         BO    EM16                                                             
         SPACE 1                                                                
         GOTO1 READ                                                             
         XC    SAVEPXPR,SAVEPXPR                                                
         LA    R3,SAVEPPRF                                                      
         LA    R5,SAVEPXPR                                                      
         BAS   RE,ANYPROF                                                       
         GOTO1 NAMOUT                                                           
         OI    LOGPRDH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         EJECT                                                                  
*              ROUTINE TO LOOK FOR PROGRAM(AC14) PROFILES                       
         SPACE 1                                                                
EM16     MVI   REV,C'N'            ASSUME NOT REVISION METHOD                   
         XC    PROFKEY,PROFKEY                                                  
         MVC   PROFKEY(4),=C'A000'                                              
         MVC   PROFKEY+2(2),=C'14'                                              
         MVC   PROFKEY+4(1),COMPANY                                             
         MVC   PROFKEY+5(2),=C'SJ'    UNIT/LEDGER                               
         MVC   PROFKEY+7(3),KEY+3  CLIENT                                       
         MVC   OFFICE,SAVEPPRF+(ACPROFFC-ACPREL)                                
         CLC   OFFICE,SPACES       TEST FOR PRODUCT LEVEL OFFICE                
         BH    *+10                YES                                          
         MVC   OFFICE,SAVECPRF+(ACPROFFC-ACPREL)                                
         CLC   OFFICE,SPACES       TEST FOR AN OFFICE                           
         BNH   EM19                NO                                           
         TM    COMPSTA4,X'01'      TEST NEW OFFICES AGENCY                      
         BO    EM18                YES                                          
         MVI   PROFKEY+10,C'*'                                                  
         MVC   PROFKEY+11(1),OFFICE                                             
         B     EM19                                                             
         SPACE 1                                                                
EM18     MVI   PROFKEY+10,C'+'                                                  
         MVC   PROFKEY+14(2),OFFICE                                             
         SPACE 1                                                                
         USING TWAD,RA                                                          
EM19     MVC   PROFKEY+12(2),TWAAGY                                             
         USING T603FFD,RA                                                       
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,PROFPARA,PROFKEY,PROGPROF,DATAMGR                       
         CLI   PROGPROF+5,0        ARE THEY USING REVISION METHOD               
         BE    EM20                NO                                           
         CLI   PROGPROF+5,C'N'                                                  
         BE    EM20                                                             
         MVI   REV,C'Y'            USE REVISION METHOD                          
         EJECT                                                                  
*              BUILD KEY (JOB ROUTINES)                                         
         SPACE 1                                                                
EM20     LA    R2,LOGJOBH                                                       
         TM    LOGJOBH+4,X'20'                                                  
         BO    EM22                                                             
         NI    LOGWCH+4,X'DF'                                                   
*&&US*&& NI    LOGREVCH+4,X'DF'                                                 
         MVC   LOGJOBN,SPACES                                                   
         OI    LOGJOBNH+6,X'80'                                                 
*&&US*&& MVC   LOGREVN,SPACES                                                   
*&&US*&& OI    LOGREVNH+6,X'80'                                                 
*&&US*&& MVC   LOGREVD,SPACES                                                   
*&&US*&& OI    LOGREVDH+6,X'80'                                                 
         MVI   FIRST,0                                                          
         BAS   RE,CLRSCR                                                        
         MVI   ESTSW,C'N'                                                       
         SPACE 1                                                                
EM22     SR    R3,R3                                                            
         IC    R3,39(RA)           LEVB LENGTH                                  
         LA    R3,KEY+3(R3)                                                     
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   0(9,R3),WORK                                                     
         MVC   JOBKEY,KEY          SAVE JOB KEY                                 
         SPACE 1                                                                
         TM    LOGJOBH+4,X'20'                                                  
         BO    EM33                                                             
         GOTO1 READ                                                             
         XC    REVN,REVN                                                        
         XC    REVD,REVD                                                        
         GOTO1 GETEL,DMCB,(X'26',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   EM30                NO REVISION NUMBER                           
*&&UK*&& B     EM30                                                             
*&&US                                                                           
         USING ACJOBD,R4                                                        
         L     R4,DMCB+12                                                       
         TM    ACJBSTAT,ACJBNEWQ   TEST FOR NEW ESTIMATING JOB                  
         BO    NEWESTS             YES-STOP INPUT                               
         OC    ACJBREV,ACJBREV                                                  
         BZ    EM30                                                             
         MVC   REVN,ACJBREV                                                     
         EDIT  (B2,ACJBREV),(4,LOGREVN),ALIGN=LEFT                              
         OI    LOGREVNH+6,X'80'                                                 
         OC    ACJBREVD,ACJBREVD                                                
         BZ    EM30                NO REVISION DATE                             
         MVC   REVD,ACJBREVD                                                    
         GOTO1 DATCON,DMCB,(1,ACJBREVD),(8,LOGREVD)                             
         OI    LOGREVDH+6,X'80'                                                 
EM30     OI    LOGREVNH+4,X'20'                                                 
         OI    LOGREVDH+4,X'20'                                                 
*&&                                                                             
*&&UK                                                                           
EM30     DS    0H                                                               
*&&                                                                             
         XC    SAVEJXPR,SAVEJXPR                                                
         LA    R3,SAVEJPRF                                                      
         LA    R5,SAVEJXPR                                                      
         BAS   RE,ANYPROF                                                       
         GOTO1 NAMOUT                                                           
         MVC   JOBNAME,SPACES                                                   
         MVC   JOBNAME(36),LOGJOBN                                              
         MVC   JOBNAME+37(5),=C'BILL='                                          
*&&UK                                                                           
         USING ACPROFD,R4                                                       
         LA    R4,SAVEJPRF                                                      
         CLI   ACPRBILL,0                                                       
         BNZ   EM31                                                             
         LA    R4,SAVEPPRF                                                      
         CLI   ACPRBILL,0                                                       
         BNZ   EM31                                                             
         LA    R4,SAVECPRF                                                      
         SPACE 1                                                                
EM31     GOTO1 ACBLTYP,DMCB,(R4),JOBNAME+42                                     
         B     EM31A                                                            
*&&                                                                             
*&&US                                                                           
         GOTO1 =A(RDOPT),DMCB,(RC),(R8),(RA),RR=RELO                            
         MVI   DOOPTNUM,OPNBT      DISPLAY BILLING TYPE                         
         LA    RE,GOBLOCK                                                       
         ST    RE,DOAGOBLK                                                      
         LA    RE,JOBNAME+42-8                                                  
         ST    RE,DOAFLDH                                                       
         MVC   DOACOM,COMFACS                                                   
         GOTO1 DISOPT,DMCB,DOBLOCK                                              
         B     EM31A                                                            
*&&                                                                             
EM31A    MVC   LOGJOBN,JOBNAME                                                  
         OI    LOGJOBNH+6,X'80'                                                 
         OI    LOGJOBH+4,X'20'                                                  
EM32     MVI   ANYKEY,C'Y'                                                      
         BAS   RE,ACTIN            BUILD TABLE OF ACTUAL CHARGES                
         B     EM33                                                             
         EJECT                                                                  
*              BUILD KEY (WORK-CODES)                                           
         SPACE 1                                                                
EM33     LA    R2,LOGWCH                                                        
         TM    LOGWCH+4,X'20'                                                   
         BO    EM34                                                             
         MVI   ANYKEY,C'Y'                                                      
         MVI   FIRST,0                                                          
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         BAS   RE,VALWC            VALIDATE WORK-CODE LINE                      
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         OI    LOGWCH+4,X'20'                                                   
*&&UK                                                                           
EM34     DS    0H                                                               
*&&                                                                             
*&&US                                                                           
EM34     LA    R2,LOGREVCH                                                      
         TM    LOGREVCH+4,X'20'                                                 
         BO    EM35                                                             
         CLI   REV,C'Y'                                                         
         BE    *+16                                                             
         CLI   5(R2),0             FIELD INVALID FOR THIS OPTION                
         BNE   INVINP                                                           
         B     EM35                                                             
         CLI   5(R2),0                                                          
         BNE   EM34A                                                            
         CLI   THISACT,NEW       IF ACTION NEW REV. CODE NOT REQUIRED           
         BE    EM35                                                             
         GOTO1 ANY                                                              
EM34A    CLI   5(R2),1                                                          
         BNE   INVINP                                                           
         CLI   LOGREVC,C'C'                                                     
         BE    EM35                CORRECTION LEAVE DATA ALONE                  
         CLI   LOGREVC,C'R'                                                     
         BNE   INVINP                                                           
         BAS   RE,REVUP            REVISION UPDATE                              
         SPACE 1                                                                
EM35     MVC   LOGREVC,SPACES                                                   
         OI    LOGREVCH+4,X'20'                                                 
         OI    LOGREVCH+6,X'80'                                                 
*&&                                                                             
         GOTO1 READ                                                             
         LA    R2,IO2                                                           
         LA    R3,IOLENQ                                                        
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         USING ACKEYD,R4                                                        
         ICM   R5,3,ACLENGTH                                                    
         MVCL  R2,R4               IO TO IO2                                    
         B     XIT                                                              
         EJECT                                                                  
EM40     DC    0H'0'                                                            
         CLI   MODE,DSPLYREC                                                    
         BE    *+8                                                              
         MVI   UPDATE,C'Y'                                                      
         GOTO1 READ                                                             
         LA    R2,IO2                                                           
         LA    R3,IOLENQ                                                        
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         USING ACKEYD,R4                                                        
         ICM   R5,3,ACLENGTH                                                    
         MVCL  R2,R4               IO TO IO2                                    
         CLI   REV,C'Y'            ARE THEY USING REVISION METHOD               
         BNE   *+10                                                             
         MVC   LOGHDL+15(8),=C'PREVIOUS'                                        
         CLI   MODE,DSPLYREC                                                    
         BNE   EM60                                                             
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 1                                                                
         USING SCRD,R6                                                          
EM50     BAS   RE,CLRSCR          CLEAR SCREEN                                  
         CLI   LASTWC,X'FF'       IF LAST WAS UNPROTECTED START AGAIN           
         BNE   EM51                                                             
         BAS   RE,VALWC            VALIDATE WORK-CODE LINE                      
         B     EM52                START AGAIN                                  
         SPACE 1                                                                
EM51     CLI   LASTWC,X'FE'   END OF LIST                                       
         BNE   EM52           NO SO PUT OUT THE REST                            
         B     EM59           DISPLAY UNPROTECTED TILL THEY ARE DONE            
         SPACE 1                                                                
EM52     ZIC   R1,LASTWC                                                        
         LA    R6,LOGFSTWH                                                      
         LA    R3,MXLINES                                                       
         ZIC   R2,WKCNT            TOTAL NUMBER OF WORK CODES                   
EM53     CR    R1,R2                                                            
         BNE   EM54                                                             
         MVI   LASTWC,X'FE'        END OF WORK CODE LIST                        
         B     EM59                                                             
EM54     SLL   R1,1                                                             
         LA    R4,WKLST(R1)                                                     
         MVC   WC,0(R4)            CODE FOR THIS LINE                           
         SRL   R1,1                                                             
         AH    R1,=H'1'                                                         
         STC   R1,LASTWC                                                        
         BAS   RE,PUTSCR           PUT OUT THE LINE                             
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,EM53             MAX OF 10 LINES                              
         CLC   LASTWC,WKCNT                                                     
         BNE   EM59                                                             
         MVI   LASTWC,X'FE'        END OF SCREEN AND OF LIST                    
         SPACE 1                                                                
EM59     LA    R2,LOGFSTOH                                                      
         GOTO1 AJOBTOTS,DMCB,(RC),(R8),(RA)                                     
         B     RECDIS              RECORD DISPLAYED                             
         EJECT                                                                  
*              BUILD UP RECORD (VALIDATE LINES)                                 
         SPACE 1                                                                
         USING SCRD,R6                                                          
EM60     LA    R6,LOGFSTWH                                                      
         CLC   SCRWC(5),=C'COPY='  COPY ESTIMATES FROM ANOTHER JOB              
         BNE   EM61                                                             
         CLI   SCRWCH+5,5                                                       
         BH    *+16                                                             
         MVI   ERROR,2                                                          
         B     EM60B                                                            
         CLI   ESTSW,C'Y'          DOES THIS JOB ALREADY HAVE ESTIMATES         
         BE    INVCOPY                                                          
         BAS   RE,COPYEST                                                       
         CLI   ERROR,OK                                                         
         BE    EM50                NOW DISPLAY THEM                             
EM60B    LA    R2,LOGFSTWH                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
EM61     BAS   RE,REVNUM           UPDATE REVISION NUMBER                       
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         LA    R3,MXLINES                                                       
EM62     CLI   SCRORGH+5,0         SEE IF ANY DATA CHANGES                      
         BE    EM64                                                             
         TM    SCRWCH+1,X'20'      IF NOT PROTECTED                             
         BO    EM63                                                             
         CLI   SCRWCH+5,0          THERE MUST BE A WORKCODE                     
         BNE   *+12                                                             
         LA    R2,SCRWCH                                                        
         B     NOINPUT             IT'S MISSING                                 
EM63     TM    SCRORGH+4,X'20'                                                  
         BNO   EM70                DATA CHANGED                                 
*&&UK                                                                           
EM64     DS    0H                                                               
*&&                                                                             
*&&US                                                                           
EM64     CLI   SCRPRVH+5,0                                                      
         BE    *+12                                                             
         TM    SCRPRVH+4,X'20'                                                  
         BNO   EM70                                                             
*&&                                                                             
         CLI   SCRCURH+5,0                                                      
         BE    *+12                                                             
         TM    SCRCURH+4,X'20'                                                  
         BNO   EM70                                                             
         CLI   SCRWCH+5,0                                                       
         BE    *+12                                                             
         TM    SCRWCH+4,X'20'                                                   
         BNO   EM70                                                             
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,EM62                                                          
         SPACE 1                                                                
         CLI   LASTWC,X'FE'                                                     
         BL    EM50                NO CHANGES - DISPLAY NEXT                    
         LA    R6,LOGFSTWH                                                      
         LA    R3,MXLINES                                                       
         MVI   LASTWC,X'FF'                                                     
EM65     TM    SCRWCH+1,X'20'                                                   
         BNO   EM50            A LINE NOT PROTECTED SO FINISHED                 
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,EM65                                                          
         MVI   LASTWC,X'FE'                                                     
         B     EM50     LAST WAS PROTECTED SO GIVE NEXT BLANK SCREEN            
         SPACE 1                                                                
         USING SCRD,R6                                                          
EM70     LA    R6,LOGFSTWH                                                      
         LA    R3,MXLINES                                                       
         ZAP   TOTINCR,=P'0'                                                    
         SPACE 1                                                                
EM72     TM    SCRORGH+4,X'20'                                                  
         BNO   EM74                                                             
         TM    SCRCURH+4,X'20'                                                  
         BNO   EM74                                                             
         TM    SCRWCH+4,X'20'                                                   
         BNO   EM74                                                             
*&&US                                                                           
         TM    SCRPRVH+1,X'20'                                                  
         BO    EM78                                                             
         TM    SCRPRVH+4,X'20'                                                  
         BNO   EM74                                                             
*&&                                                                             
         B     EM78                NOT CHANGED                                  
         SPACE 1                                                                
EM74     MVC   WC,SCRWC                                                         
         OC    WC,SPACES                                                        
         CLC   WC,SPACES                                                        
         BE    EM80                NO MORE WORK CODES                           
         TM    SCRWCH+1,X'20'                                                   
         BO    EM76                IF PROTECTED NO NEED TO VALIDATE             
         LA    R2,SCRWCH                                                        
         CLI   5(R2),2                                                          
         BH    INVINP                                                           
         GOTO1 GETEL,DMCB,(X'35',IO2),(2,WC)                                    
         CLI   DMCB+12,0                                                        
         BE    WCONEST             ITS ON ESTIMATE                              
         BAS   RE,WCNAME           VALIDATE CODE                                
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         BAS   RE,ACTOUT           AND ACTUAL CHARGES                           
EM76     BAS   RE,BLDELM           BUILD A NEW ELEMENT                          
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         CLI   PROGPROF+6,C'Y'                                                  
         BNE   *+8                                                              
         MVI   CHNGSW,C'Y'         RECORD WAS CHANGED                           
         TM    SCRWCH+1,X'20'                                                   
         BO    EM78                ALREADY PROTECTED                            
         MVC   SCRDES,WCNME         PUT OUT NAME                                
         OI    SCRWCH+1,X'20'                                                   
         OI    SCRWCH+4,X'20'                                                   
         OI    SCRWCH+6,X'80'                                                   
         SPACE 1                                                                
EM78     OI    SCRORGH+4,X'20'                                                  
         OI    SCRORGH+6,X'80'                                                  
         OI    SCRCURH+4,X'20'                                                  
         OI    SCRCURH+6,X'80'                                                  
*&&US                                                                           
         OI    SCRPRVH+4,X'20'                                                  
         OI    SCRPRVH+6,X'80'                                                  
*&&                                                                             
         LA    R6,SCRLEN(R6)       NEXT LINE                                    
         BCT   R3,EM72                                                          
         SPACE 1                                                                
EM80     GOTO1 AJOBTOTS,DMCB,(RC),(R8),(RA)                                     
         SPACE 1                                                                
         MVC   KEY,JOBKEY                                                       
         GOTO1 READ                                                             
         CLI   CHNGSW,C'Y'         SOMETHING'S CHANGED AND PROFILE              
         BNE   EM82                OPTION IS ON                                 
         MVI   CHNGSW,C'N'                                                      
         GOTO1 GETEL,DMCB,(X'26',IO2),0                                         
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                NO JOB ELEMENT                               
         L     R4,DMCB+12                                                       
         USING ACJOBD,R4                                                        
         OI    ACJBSTAT,X'80'      NOTE ESTIMATE=UNAPPROVED                     
         B     EMPUT                                                            
         DROP  R4                                                               
         SPACE 1                                                                
*        LA    R4,IO2                                                           
*        AH    R4,DATADISP                                                      
*        SR    R5,R5                                                            
*EM18    CLI   0(R4),0                                                          
*        BE    EM81B                                                            
*        CLI   0(R4),X'3C'        SEARCH FOR 3C ELEMENT                         
*        BE    EM81A                                                            
*        IC    R5,1(R4)                                                         
*        AR    R4,R5                                                            
*        B     EM81                                                             
*        USING ACXPROFD,R4                                                      
*EM81A   OI    ACXPST1,X'04'       TURN ON UNAPPROVED BIT IN RECORD             
*        B     EMPUT                                                            
*EM81B   LA    R4,ELEMENT          NO 3C ELEMENT - ADD ONE                      
*        MVC   ELEMENT(L'STANDX),STANDX                                         
*        OI    ACXPST1,X'04'                                                    
*        GOTO1 ADDANEL                                                          
*        B     EMPUT               PUT THE RECORD                               
EM82     LA    RF,IO2                                                           
         CLC   IO+42(2),42(RF)     NO UNAPPORVED CHANGES - SEE IF               
         BNE   EMPUT               RECORD HAS CHANGED                           
         LA    R2,IO2              IF LENGTHS ARE THE SAME                      
*                                  COMPARE ENTIRE RECORD                        
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         USING ACKEYD,R4                                                        
         ICM   R5,3,ACLENGTH                                                    
         LR    R3,R5                                                            
         CLCL  R2,R4                                                            
         BE    EM83                                                             
EMPUT    GOTO1 PUTREC                                                           
         BAS   RE,REQST            TURNAROUND REQUEST                           
EM83     LA    R6,LOGFSTWH                                                      
         LA    R3,MXLINES                                                       
EM84     TM    SCRWCH+1,X'20'                                                   
         BNO   NOMORE          A LINE NOT PROTECTED SO FINISHED                 
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,EM84                                                          
         B     RECCHA              RECORD CHANGED                               
         EJECT                                                                  
*              ROUTINE TO EDIT INPUT AND BUILD ELEMENT                          
         SPACE 1                                                                
BLDELM   NTR1                                                                   
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING ACESTD,R4                                                        
         MVC   ACESTEL(2),=X'3510'                                              
         MVC   ACESTWRK,WC                                                      
         ZAP   ACESTORG,=P'0'                                                   
         ZAP   ACESTCUR,=P'0'                                                   
         LA    R2,SCRORGH                                                       
         GOTO1 ANY                                                              
         BAS   RE,VALSTRNG                                                      
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         MVC   ACESTCUR,DUB+2                                                   
         MVC   ACESTORG,DUB+2                                                   
         LA    R2,SCRCURH                                                       
         CLI   5(R2),0                                                          
         BE    BLDELM1                                                          
         BAS   RE,VALSTRNG                                                      
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         MVC   ACESTCUR,DUB+2                                                   
         AP    TOTINCR,ADDONS                                                   
         SPACE 1                                                                
*&&UK                                                                           
BLDELM1  DS    0H                                                               
*&&                                                                             
*&&US                                                                           
BLDELM1  TM    SCRPRVH+1,X'20'                                                  
         BO    BLDELM3                                                          
         MVI   ACESTLEN,22                                                      
         ZAP   ACESTPRV,=P'0'                                                   
         LA    R2,SCRPRVH                                                       
         CLI   5(R2),0                                                          
         BE    BLDELM3                                                          
         BAS   RE,VALSTRNG                                                      
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
         MVC   ACESTPRV,DUB+2                                                   
*&&                                                                             
BLDELM3  GOTO1 DELEL,DMCB,(X'35',IO2),(2,WC)                                    
         CP    ACESTCUR,=P'0'                                                   
         BNE   BLDELM5                                                          
         CP    ACESTORG,=P'0'                                                   
         BNE   BLDELM5                                                          
         CLI   ACESTLEN,22                                                      
         BNE   XIT                 ORG AND CURRENT ZERO                         
         CP    ACESTPRV,=P'0'                                                   
         BE    XIT                                                              
         SPACE 1                                                                
BLDELM5  EDIT  ACESTORG,(12,SCRORG),2,ALIGN=LEFT,FLOAT=-                        
         EDIT  ACESTCUR,(12,SCRCUR),2,ALIGN=LEFT,FLOAT=-                        
         CLI   ACESTLEN,22                                                      
         BL    BLDELM6                                                          
*&&US*&& EDIT  ACESTPRV,(12,SCRPRV),2,ALIGN=LEFT,FLOAT=-                        
BLDELM6  GOTO1 ADDEL,DMCB,IO2,(R4)                                              
         CLI   DMCB+12,5                                                        
         BE    BIGREC                                                           
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                SOMETHING FUNNY                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE WORK-CODE LINE                               
         SPACE 1                                                                
VALWC    NTR1                                                                   
         GOTO1 READ                                                             
         LA    R2,IO2                                                           
         LA    R3,IOLENQ                                                        
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         USING ACKEYD,R4                                                        
         ICM   R5,3,ACLENGTH                                                    
         MVCL  R2,R4               IO TO IO2                                    
         LA    R2,LOGWCH                                                        
         SR    R3,R3                                                            
         XC    WKLST,WKLST                                                      
         MVI   WKCNT,0                                                          
         MVI   LASTWC,0                                                         
         LA    R5,1                                                             
         CLI   5(R2),0                                                          
         BE    VALWC10             IF NO INPUT USE ALL                          
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CSCANNER,DMCB,(R2),(10,BLOCK),C',=,-'                            
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    INVINP              INVALID INPUT                                
         LA    R6,BLOCK                                                         
         SPACE 1                                                                
VALWC3   AH    R3,=H'1'                                                         
         CLI   12(R6),C'+'         ONLY LOOK AT NEW ONES FOR NOW                
         BNE   VALWC4                                                           
         CLI   1(R6),0                                                          
         BNE   INVFLD                                                           
         CLI   0(R6),3                                                          
         BH    INVFLD              INVALID INPUT FIELD #                        
         CLI   0(R6),2                                                          
         BL    INVFLD              INVALID INPUT FIELD #                        
         MVC   WC,13(R6)           +WORK-CODE                                   
         BAS   RE,BLDLST           ADD VALID CODES TO WKLST                     
         CLI   ERROR,OK                                                         
         BNE   ERRXIT                                                           
VALWC4   LA    R6,32(R6)           NEXT ENTRY IN SCAN BLOCK                     
         BCT   R5,VALWC3                                                        
         SPACE 1                                                                
VALWC10  BAS   RE,ESTWC            ADD WORK-CODES ON ESTIMATE                   
         BAS   RE,ACTWC            ADD ACTUAL WORK-CODES                        
         BAS   RE,TWAWRT           SAVE TWA                                     
         CLI   WKCNT,0                                                          
         BNE   XIT                                                              
         B     NODATA                                                           
         EJECT                                                                  
*              ROUTINE TO BUILD LIST OF VALID CODES                             
         SPACE 1                                                                
BLDLST   NTR1                                                                   
         LA    R2,LOGWCH                                                        
         BAS   RE,WCNAME           DOES CODE EXIST                              
         CLI   ERROR,OK                                                         
         BNE   ERRXIT              BAD WORK-CODE                                
         BAS   RE,ADDLST           ADD IT TO LIST                               
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD CODES FOR ITEMS ALREADY ON ESTIMATE               
ESTWC    NTR1                                                                   
         GOTO1 GETEL,DMCB,(X'35',IO2),0                                         
         CLI   DMCB+12,0                                                        
         BNE   XIT                 NOTHING ON ESTIMATE                          
         L     R4,DMCB+12                                                       
         USING ACESTD,R4                                                        
ESTWC2   MVC   WC,ACESTWRK                                                      
         BAS   RE,FILTER           SEE IF IT QUALIFIES-IF SO ADD IT             
ESTWC4   ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),X'35'                                                      
         BE    ESTWC2                                                           
         CLI   0(R4),0                                                          
         BNE   ESTWC4                                                           
         B     XIT                                                              
         SPACE 1                                                                
*               ROUTINE TO ADD ACTUAL WORK-CODES                                
         SPACE 1                                                                
         USING CHARD,R5                                                         
ACTWC    NTR1                                                                   
         LA    R5,CHARGES          LOOK UP CHARGES                              
         ZIC   R3,CHRCNT                                                        
         LTR   R3,R3                                                            
         BZ    XIT                                                              
ACTWC2   MVC   WC,CHARCD                                                        
         BAS   RE,FILTER           SEE IF IT QUALIFIES-IF SO ADD IT             
         LA    R5,CHARLNG(R5)                                                   
         BCT   R3,ACTWC2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              FILTER WORK-CODES AGAINST INPUT LIST                             
         SPACE 1                                                                
FILTER   NTR1                                                                   
         LA    R2,LOGWCH                                                        
         LA    R5,1                                                             
         CLI   5(R2),0                                                          
         BE    FILTER20            IF NO INPUT USE ALL                          
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CSCANNER,DMCB,(R2),(10,BLOCK),C',=,-'                            
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    INVINP              INVALID INPUT                                
         LR    R3,R5                                                            
         LA    R6,BLOCK                                                         
FILTER1  CLI   12(R6),C'+'                                                      
         BNE   FILTER2                                                          
         LA    R6,32(R6)                                                        
         BCT   R3,FILTER1                                                       
         BAS   RE,ADDLST                                                        
         B     XIT                 ALL INPUT IS +WC SO USE ALL                  
         SPACE 1                                                                
FILTER2  LA    R6,BLOCK                                                         
         SR    R3,R3                                                            
         SPACE 1                                                                
FILTER3  AH    R3,=H'1'                                                         
         MVC   LOW,=X'0000'                                                     
         MVC   HI,=X'FFFF'                                                      
         CLI   12(R6),C'+'                                                      
         BE    FILTER21                                                         
         CLI   0(R6),2                                                          
         BH    INVFLD              INVALID INPUT FIELD #                        
         CLI   1(R6),2                                                          
         BH    INVFLD              HIGH RANGE IS INVALID                        
         MVC   LOW,12(R6)          LOW RANGE                                    
         CLI   1(R6),0                                                          
         BNE   FILTER4                                                          
         MVC   22(2,R6),12(R6)                                                  
FILTER4  MVC   HI,22(R6)                                                        
         CLC   LOW,HI              START HIGHER THAN END                        
         BH    INVFLD                                                           
         CH    R3,=H'1'                                                         
         BE    FILTER10            NO CHECK FOR DUPLICATE IF FIRST              
         LR    R2,R3                                                            
         BCTR  R2,0                NUMBER SO FAR - LESS ONE                     
         LA    R4,BLOCK                                                         
FILTER7  CLI   12(R4),C'+'                                                      
         BE    FILTER9                                                          
         CLC   LOW,22(R4)                                                       
         BH    FILTER9                                                          
         CLC   HI,12(R4)                                                        
         BNL   INVDUP              DUP. OR OVERLAPPING                          
FILTER9  LA    R4,32(R4)           NEXT BLOCK LIST                              
         BCT   R2,FILTER7                                                       
         SPACE 1                                                                
FILTER10 CLC   WC,LOW              IS IT WITHIN RANGE                           
         BL    FILTER21            SKIP IT                                      
         LA    R1,1                                                             
         CLI   HI+1,C' '           FOR A-B                                      
         BNE   *+6                 SHOW ALL THAT START WITH B                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WC(0),HI                                                         
         BH    FILTER21                                                         
FILTER20 BAS   RE,ADDLST           YES, SO ADD IT TO LIST                       
FILTER21 LA    R6,32(R6)           NEXT ENTRY IN SCAN BLOCK                     
         BCT   R5,FILTER3                                                       
         B     XIT                                                              
         SPACE 1                                                                
ADDLST   NTR1                                                                   
         ZIC   R2,WKCNT            NUMBER IN LIST                               
         LA    R0,MXWKCNT          MAX                                          
         CR    R2,R0                                                            
         BNL   XIT                 IF TABLE FULL SKIP IT                        
         GOTO1 BINSRCH,DMCB,(1,WC),WKLST,(R2),2,(0,2),(R0)                      
         OC    DMCB(4),DMCB                                                     
         BZ    XIT                 TABLE IS FULL                                
         MVC   WKCNT,DMCB+11       UPDATE COUNT                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE CASH STRING                                  
         SPACE 1                                                                
VALSTRNG NTR1                                                                   
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CSCANNER,DMCB,(R2),(10,BLOCK)                                    
         SR    R3,R3                                                            
         IC    R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    INVINP                                                           
         LA    R4,BLOCK                                                         
         ZAP   DUB,=P'0'                                                        
         ZAP   ADDONS,=P'0'                                                     
         MVI   ADDFST,C'Y'                                                      
         SPACE 1                                                                
VAL2     CLI   1(R4),0                                                          
         BNE   INVINP                                                           
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+7(1),0(R4)     PASS LENGTH OF THIS ITEM                     
         GOTO1 CASHVAL,DMCB,12(R4)                                              
         CLI   DMCB,X'FF'                                                       
         BE    INVINP                                                           
         L     R5,DMCB+4                                                        
         CVB   R6,DUB                                                           
         CVD   R5,DUB                                                           
         CLI   ADDFST,C'Y'                                                      
         BE    *+10                                                             
         AP    ADDONS,DUB                                                       
         MVI   ADDFST,C'N'                                                      
         AR    R6,R5                                                            
         CVD   R6,DUB                                                           
         LA    R4,32(R4)                                                        
         BCT   R3,VAL2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK FOR PROFILES                                    
         SPACE 1                                                                
ANYPROF  NTR1                                                                   
         XC    0(L'SAVECPRF,R3),0(R3)                                           
         XC    0(L'SAVECXPR,R5),0(R5)                                           
         GOTO1 GETEL,DMCB,(X'24',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   ANYP2                                                            
         L     R4,DMCB+12                                                       
         MVC   0(L'SAVECPRF,R3),0(R4)                                           
         SPACE 1                                                                
ANYP2    GOTO1 GETEL,DMCB,(X'3C',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   XIT                                                              
         L     R4,DMCB+12                                                       
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD A LINE ON SCREEN                                
*              R6 TO FIRST HEADER OF LINE                                       
         SPACE 1                                                                
         USING SCRD,R6                                                          
         USING ACESTD,R4                                                        
PUTSCR   NTR1                                                                   
         LA    R2,SCRORGH                                                       
         MVC   SCRWC,WC                                                         
         OI    SCRWCH+1,X'20'      PROTECT IT                                   
         OI    SCRWCH+6,X'80'                                                   
         LA    R2,SCRWCH                                                        
         BAS   RE,WCNAME           GET W/C NAME                                 
         MVC   SCRDES,WCNME                                                     
         MVI   SCRORG,C'0'                                                      
         GOTO1 GETEL,DMCB,(X'35',IO2),(2,WC)                                    
         CLI   DMCB+12,0                                                        
         BNE   PUTSCR3                                                          
         L     R4,DMCB+12                                                       
         EDIT  ACESTORG,(12,SCRORG),2,ALIGN=LEFT,FLOAT=-                        
         EDIT  ACESTCUR,(12,SCRCUR),2,ALIGN=LEFT,FLOAT=-                        
         CLI   REV,C'Y'                                                         
         BNE   PUTSCR3                                                          
         CLI   ACESTLEN,22                                                      
         BL    PUTSCR3                                                          
*&&US*&& EDIT  ACESTPRV,(12,SCRPRV),2,ALIGN=LEFT,FLOAT=-                        
PUTSCR3  BAS   RE,ACTOUT           GET ACTUAL                                   
         OI    SCRORGH+4,X'20'                                                  
         OI    SCRORGH+6,X'80'                                                  
         OI    SCRCURH+4,X'20'                                                  
         OI    SCRCURH+6,X'80'                                                  
*&&US*&& OI    SCRPRVH+4,X'20'                                                  
*&&US*&& OI    SCRPRVH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD TABLE OF ACTUAL CHARGES                         
         SPACE 1                                                                
         USING CHARD,R5                                                         
ACTIN    NTR1                                                                   
         LA    R5,CHARGES                                                       
         LA    R3,MXCHRG                                                        
ACTIN1   XC    CHARCD(CHARLNG),CHARCD                                           
         LA    R5,CHARLNG(R5)                                                   
         BCT   R3,ACTIN1                                                        
         LA    R5,CHARGES                                                       
         SR    R3,R3                                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE,KEY                                                      
         BE    *+6                                                              
         DC    H'0'                CAN'T READ JOB                               
         SPACE 1                                                                
ACTIN3   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCOUNT',KEY,IO                       
         CLI   DMCB+8,0                                                         
         BNE   ACTIN11                                                          
         MVC   KEY,IO                                                           
         CLC   KEYSAVE(15),KEY                                                  
         BNE   ACTIN11             END OF JOB                                   
         GOTO1 GETEL,DMCB,(X'44',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   ACTIN3                                                           
         L     R4,DMCB+12                                                       
         USING TRANSD,R4                                                        
         CLC   TRNSANAL,=C'99'                                                  
         BE    ACTIN3                                                           
         CLC   TRNSANAL,=C'**'                                                  
         BE    ACTIN3                                                           
         CLC   TRNSANAL,SPACES                                                  
         BE    ACTIN3                                                           
         TM    TRNSSTAT,X'80'                                                   
         BNO   ACTIN3                                                           
         CLC   IO+15(2),CHARCD          WORK-CODE MATCH                         
         BE    ACTIN9                                                           
         LA    R0,MXCHRG                                                        
         CR    R3,R0                    ALL READY HAVE MAX                      
         BE    ACTIN9                   MAKE IT   OTHERS                        
         OC    CHARCD,CHARCD                                                    
         BZ    *+8                      FIRST TIME                              
         LA    R5,CHARLNG(R5)                                                   
         MVC   CHARCD,IO+15             WORK CODE TO TABLE                      
         ZAP   CHARAMT,TRNSAMNT         AMOUNT TO TABLE                         
         LA    R3,1(R3)                                                         
         B     ACTIN3                                                           
         SPACE 1                                                                
ACTIN9   AP    CHARAMT,TRNSAMNT                                                 
         B     ACTIN3                                                           
         SPACE 1                                                                
ACTIN11  STC   R3,CHRCNT           NUMBER OF W/C CHARGES                        
         MVC   KEY,KEYSAVE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT OUT CHARGES FOR ESTIMATE                          
*              R6 A(FIRST HEADER ON LINE)                                       
         SPACE 1                                                                
         USING CHARD,R5                                                         
         USING SCRD,R6                                                          
ACTOUT   NTR1                                                                   
         LA    R5,CHARGES                                                       
         ZIC   R3,CHRCNT                                                        
         LTR   R3,R3                                                            
         BZ    XIT                 NO CHARGES FOR JOB                           
ACTOUT2  CLC   WC,CHARCD                                                        
         BE    ACTMTCH             FOUND THE CHARGES                            
         LA    R5,CHARLNG(R5)                                                   
         BCT   R3,ACTOUT2                                                       
         B     XIT                 NO CHARGES FOR ESTIMATE                      
ACTMTCH  CP    CHARAMT,=P'0'                                                    
         BH    ACTMTCH1            IF POSITIVE NEED EXTRA CHARACTER             
         EDIT  CHARAMT,(10,SCRACT),2,MINUS=YES                                  
         B     ACTMTCH2                                                         
ACTMTCH1 EDIT  CHARAMT,(10,SCRACT),2                                            
ACTMTCH2 OI    SCRACTH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLEAR  SCREEN                                         
*              R3 NUMBER OF LINES REMAINING                                     
*              R6 A(FIRST HEADER ON LINE)                                       
         SPACE 1                                                                
         USING SCRD,R6                                                          
CLRSCR   NTR1                                                                   
         LA    R3,MXLINES                                                       
         LA    R6,LOGFSTWH                                                      
CLRSCR1  TWAXC SCRWCH,SCRACTH,PROT=Y                                            
         NI    SCRWCH+1,X'DF'      UNPROTECT WORK CODE                          
*&&US                                                                           
         OI    SCRPRVH+1,X'20'     PROTECT PREVIOUS                             
         CLI   REV,C'Y'            ARE THEY USING REVISION METHOD               
         BNE   *+8                                                              
         NI    SCRPRVH+1,X'DF'     UNPROTECT PREVIOUS                           
         OI    SCRPRVH+6,X'80'                                                  
*&&                                                                             
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,CLRSCR1                                                       
*&&US                                                                           
         MVC   LOGTOT1,SPACES                                                   
         OI    LOGTOT1H+6,X'80'                                                 
         MVC   LOGTOT2,SPACES                                                   
         OI    LOGTOT2H+6,X'80'                                                 
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     RF,DMCB                                                          
         USING FACTSD,RF                                                        
         TM    FATSTAT,X'10'                                                    
         BO    *+10                3270 IS OK                                   
         MVC   LOGLAST+1(2),=X'0101' 2260 IS A PAIN IN THE ASS                  
*&&                                                                             
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE WORK-CODE                                    
         SPACE 1                                                                
WCNAME   NTR1                                                                   
         MVC   WCNME,=CL15'CODE IS INVALID'                                     
         MVC   KEY,SPACES          GO AND VALIDATE WORK CODE                    
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),36(RA)                                                  
         MVC   KEY+4(2),WC                                                      
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BNE   WCNOTFD                                                          
         GOTO1 GETEL,DMCB,(X'12',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   WCNOTFD                                                          
         L     R4,DMCB+12                                                       
         USING ACANALD,R4                                                       
         MVC   WCNME,ACANDESC                                                   
         MVC   KEY,JOBKEY          RESTORE JOB KEY                              
         B     XIT                                                              
         SPACE 1                                                                
TWAWRT   NTR1                                                                   
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVC   DMCB+8(2),=X'0100'  AND PAGE NUMBER                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,TWA1                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COPY ESTIMATES FROM ANOTHER JOB                       
         SPACE 2                                                                
*                                  R6=A(FIELD)                                  
COPYEST  NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),36(RA)     ACMPJOB                                      
         ZIC   R1,5(R6)            LENGTH OF FIELD                              
         SH    R1,=H'6'            LESS 'COPY=' + 1                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),13(R6)                                                  
         GOTO1 HIGH                READ 'FROM' ACCOUNT                          
         CLC   KEY,KEYSAVE                                                      
         BE    *+12                                                             
         MVI   ERROR,17            INVALID ACCOUNT                              
         B     XIT                                                              
         ZIC   R1,38(RA)           LEVEL A LENGTH                               
         ZIC   RF,39(RA)           LEVEL B LENGTH                               
         LA    R1,5(RF,R1)         PLUS 'COPY='                                 
         STC   R1,WORK                                                          
         CLC   5(1,R6),WORK                                                     
         BH    *+12                                                             
         MVI   ERROR,103           WRONG LEVEL ACCOUNT                          
         B     XIT                                                              
         GOTO1 GETEL,DMCB,(X'35',IO),0    GET AN ESTIMATE ELEMENT               
         CLI   DMCB+12,0                                                        
         BNE   COPYX                                                            
         L     R4,DMCB+12                                                       
COPY2    GOTO1 ADDEL,DMCB,IO2,(R4)        ADD ALL ESTIMATES TO 'KEY'            
         CLI   DMCB+12,5                                                        
         BE    BIGREC                                                           
         CLI   DMCB+12,0                                                        
         BE    COPY4                                                            
         DC    H'0'                COULDN'T ADD 35 ELEMENT                      
COPY4    ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),X'35'                                                      
         BE    COPY2                                                            
         CLI   0(R4),0                                                          
         BNE   COPY4                                                            
COPYX    XC    LOGWC,LOGWC                                                      
         OI    LOGWCH+6,X'80'      TRANSMIT CLEARED FIELD                       
         MVC   KEY,JOBKEY          REREAD KEY JOB                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 READ                                                             
         GOTO1 PUTREC                                                           
         BAS   RE,REQST                                                         
         MVI   LASTWC,X'FF'        SET LASTWC FOR REDISPLAY                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVE CURRENT EST. TO PREVIOUS                         
         SPACE 1                                                                
REVUP    NTR1                                                                   
         MVC   KEY,JOBKEY                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 READ                                                             
REVUP1   LA    R4,IO                                                            
         AH    R4,DATADISP                                                      
REVUP2   CLI   0(R4),0                                                          
         BE    REVUP5                                                           
         CLI   0(R4),X'35'                                                      
         BNE   REVUP3                    FIX ELEMENTS                           
         USING ACESTD,R4                                                        
         CLI   ACESTLEN,X'10'                                                   
         BH    REVUP3              ALREADY FIXED                                
         ZIC   R3,ACESTLEN                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R4)                                                 
         MVI   0(R4),X'FF'                                                      
         GOTO1 DELEL,DMCB,(X'FF',IO),0  DELETE OLD ELEMENT                      
         LA    R4,ELEMENT                                                       
         MVI   ACESTLEN,22                                                      
         ZAP   ACESTPRV,=P'0'                                                   
         GOTO1 ADDEL,DMCB,IO,ELEMENT    ADD NEW ELEMENT                         
         B     REVUP1                   DO NEXT ELEMENT                         
REVUP3   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     REVUP2                                                           
         SPACE 1                                                                
REVUP5   LA    R2,IO2                                                           
         LA    R3,IOLENQ                                                        
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         USING ACKEYD,R4                                                        
         ICM   R5,3,ACLENGTH                                                    
         MVCL  R2,R4               IO TO IO2                                    
         GOTO1 GETEL,DMCB,(X'35',IO2),0                                         
         CLI   DMCB+12,0                                                        
         BNE   XIT                                                              
         L     R4,DMCB+12                                                       
         USING ACESTD,R4                                                        
REVUP6   ZAP   ACESTPRV,ACESTCUR                                                
REVUP7   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'35'                                                      
         BE    REVUP6                                                           
         CLI   0(R4),0                                                          
         BNE   REVUP7                                                           
         GOTO1 WRITE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              UPDATE REVISION NUMBER AND DATE                                  
         SPACE 1                                                                
REVNUM   NTR1                                                                   
*&&UK*&& B     XIT                                                              
*&&US                                                                           
         TM    LOGREVNH+4,X'20'                                                 
         BNO   REVNUM1                                                          
         TM    LOGREVDH+4,X'20'                                                 
         BNO   REVNUM1                                                          
         B     REVXIT              REVISION NUMBER/DATE NOT CHANGED             
REVNUM1  XC    REVN,REVN                                                        
         XC    REVD,REVD                                                        
         CLI   LOGREVNH+5,0                                                     
         BE    REVNUM2                                                          
         TM    LOGREVNH+4,X'08'    INSURE VALID NUMERIC                         
         BZ    INVNUM                                                           
         ZIC   R1,LOGREVNH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LOGREVN(0)                                                   
         CVB   R3,DUB                                                           
         STCM  R3,3,REVN                                                        
REVNUM2  LA    R2,LOGREVDH                                                      
         CLI   LOGREVDH+5,0                                                     
         BE    REVNUM5                                                          
         OC    REVN,REVN                                                        
         BZ    NEEDREV             NEED REVISION NUMBER WITH DATE               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    INVDTE                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,REVD)                                    
REVNUM5  LA    R4,ELEMENT                                                       
         USING ACJOBD,R4                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACJBEL,X'26'                                                     
         MVI   ACJBLEN,ACJBLNQ3                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,ACJBSTRT)                                   
         MVC   ACJBCLOS,ACJBSTRT                                                
         MVC   ACJBREV,REVN                                                     
         MVC   ACJBREVD,REVD                                                    
         GOTO1 GETEL,DMCB,(X'26',IO2),0                                         
         CLI   DMCB+12,0                                                        
         BNE   REVNUM6             NO JOB ELEMENT                               
         L     R5,DMCB+12                                                       
         MVC   ACJBCLOS,ACJBCLOS-ACJOBD(R5)   SAVE OLD CLOSE DATE               
         MVC   ACJBSTRT,ACJBSTRT-ACJOBD(R5)   SAVE OLD START DATE               
         CLI   ACJBLEN-ACJOBD(R5),ACJBLNQ2                                      
         BL    REVNUM5A                       NO OLD OPEN DATE                  
         MVC   ACJBOPND,ACJBOPND-ACJOBD(R5)   SAVE OLD START DATE               
         CLI   ACJBLEN-ACJOBD(R5),ACJBLNQ3                                      
         BL    REVNUM5A                       NO OLD FILT2                      
         MVC   ACJBFLT2(ACJBLNQ3-ACJBLNQ2),ACJBFLT2-ACJOBD(R5)                  
REVNUM5A MVI   0(R5),X'FF'                    DELETE OLD FROM RECORD            
         GOTO1 DELEL,DMCB,(X'FF',IO2),0                                         
REVNUM6  GOTO1 ADDEL,DMCB,IO2,(R4)                                              
         MVC   KEY,JOBKEY                                                       
         GOTO1 READ                                                             
         GOTO1 PUTREC                                                           
         BAS   RE,REQST                                                         
REVXIT   OI    LOGREVNH+4,X'20'                                                 
         OC    REVN,REVN                                                        
         BNZ   *+14                                                             
         MVC   LOGREVN,SPACES                                                   
         OI    LOGREVNH+6,X'80'                                                 
         OI    LOGREVDH+4,X'20'                                                 
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
         USING COMFACSD,RF                                                      
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         L     RF,COMFACS                                                       
         GOTO1 CHELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         L     RF,COMFACS                                                       
         GOTO1 CHELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     RF,COMFACS                                                       
         GOTO1 CHELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                        
         B     XIT                                                              
         EJECT                                                                  
*              PUT OUT REQUEST                                                  
         SPACE 1                                                                
REQST    NTR1                                                                   
*&&US                                                                           
         GOTO1 =A(RDOPT),DMCB,(RC),(R8),(RA),RR=RELO                            
         CLI   GOAUTOTA,C'Y'       TEST FOR T/A OPTION                          
         BE    REQST2              YES                                          
         B     XIT                 NO                                           
*&&                                                                             
*&&UK                                                                           
         TM    SAVESTAT,X'01'      NO ESTIMTE TURNAROUND                        
         BO    XIT                                                              
         LA    R4,SAVECXPR         CLIENT,PROD OR JOB OPTION TO                 
         USING ACXPROFD,R4                                                      
         TM    ACXPST1,X'80'       SUPPRESS ESTIMATE TURNAROUND                 
         BO    XIT                                                              
         LA    R4,SAVEPXPR                                                      
         TM    ACXPST1,X'80'                                                    
         BO    XIT                                                              
         LA    R4,SAVEJXPR                                                      
         TM    ACXPST1,X'80'                                                    
         BO    XIT                                                              
*&&                                                                             
         SPACE 1                                                                
REQST2   MVC   WORK,SPACES         WRITE OUT A REQUEST FOR ESTIMATES            
         MVC   WORK(2),=C'14'                                                   
         TM    SAVESTA2,X'80'      TURNAROUND ON PLAIN PAPER                    
         BZ    *+8                                                              
         MVI   WORK+1,C'5'         AC15                                         
         MVC   WORK+9(15),IO2                                                   
         MVI   WORK+59,C'S'        PRESET TO SUPPRESS                           
         TM    SAVESTA2,X'40'      NOT SUPPRESSED                               
         BZ    *+8                                                              
         MVI   WORK+59,C' '                                                     
         TM    SAVESTA2,X'10'      DIFFERENCE OPTION                            
         BZ    *+8                                                              
         MVI   WORK+59,C'D'                                                     
*&&US*&& MVC   WORK+66(12),=C'*TURNAROUND*'   FLAG TURNAROUNDS                  
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,14                                                    
         MVC   ELEMENT+26(80),WORK                                              
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCREQS',                     X        
               ELEMENT,ELEMENT,(TERMINAL,0)                                     
         B     XIT                                                              
         EJECT                                                                  
*              ERROR HANDLING ROUTINES                                          
         SPACE 1                                                                
INVINP   MVI   ERROR,2                                                          
         B     CURSOR                                                           
INVFLD   MVC   LOGHEAD(L'INVFMSG),INVFMSG                                       
         LA    R4,LOGHEAD+L'INVFMSG                                             
INVF2    EDIT  (R3),(2,0(R4)),ALIGN=LEFT                                        
INVF3    LA    R2,LOGWCH                                                        
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
INVDUP   MVC   LOGHEAD(L'INVDMSG),INVDMSG                                       
         LA    R4,LOGHEAD+L'INVDMSG                                             
         B     INVF2                                                            
         SPACE 1                                                                
WCNOTFD  MVC   LOGHEAD(L'WCNOTMSG),WCNOTMSG                                     
         MVC   LOGHEAD+20(2),WC                                                 
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
WCONEST  MVC   LOGHEAD(L'WCONEMSG),WCONEMSG                                     
         MVC   LOGHEAD+20(2),WC                                                 
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
NODATA   MVC   LOGHEAD(L'NODATMSG),NODATMSG                                     
         OI    LOGWCH+4,X'20'                                                   
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGFSTWH                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
NEEDREV  MVC   LOGHEAD(L'NDREVMSG),NDREVMSG                                     
         MVI   ERROR,X'FE'                                                      
*&&US*&& LA    R2,LOGREVNH                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
NOINPUT  MVI   ERROR,1                                                          
         B     CURSOR                                                           
         SPACE 1                                                                
INVDTE   MVI   ERROR,13            INVALID DATE FORMAT                          
         B     CURSOR                                                           
         SPACE 1                                                                
INVNUM   MVI   ERROR,3             FIELD NOT NUMERIC                            
*&&US*&& LA    R2,LOGREVNH                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
BIGREC   MVI   ERROR,66                                                         
         LA    R2,LOGRECH                                                       
         B     CURSOR                                                           
         SPACE 1                                                                
RECDIS   TM    LOGFSTWH+1,X'20'                                                 
         BNO   NODATA                                                           
         MVC   LOGHEAD(L'RECDSMSG),RECDSMSG                                     
         MVI   ERROR,X'FE'                                                      
*&&US*&& LA    R2,LOGREVNH                                                      
         CLI   FIRST,0                                                          
         BE    *+8                                                              
         LA    R2,LOGFSTOH                                                      
         MVI   FIRST,1                                                          
         B     CURSOR                                                           
         SPACE 1                                                                
RECCHA   MVC   LOGHEAD(L'RECCHMSG),RECCHMSG                                     
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGFSTOH                                                      
         TM    LOGFSTWH+1,X'20'                                                 
         BO    *+8                                                              
         LA    R2,LOGFSTWH                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
NOMORE   MVC   LOGHEAD(L'NOMORMSG),NOMORMSG                                     
         MVI   LASTWC,X'FF'      LAST LINE NOT PROTECTED START AGAIN            
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGCLIH                                                       
         B     CURSOR                                                           
         SPACE 1                                                                
INVCOPY  MVC   LOGHEAD(L'INVCOPYM),INVCOPYM                                     
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
NEWESTS  MVC   LOGHEAD(L'NEWESMSG),NEWESMSG                                     
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
CURSOR   OI    6(R2),X'40'                                                      
ERRXIT   XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
INVFMSG  DC    C'**ERROR** INVALID INPUT FIELD #'                               
INVDMSG  DC    C'**ERROR** DUPLICATE OR OVERLAPPING CODES FIELD #'              
WCNOTMSG DC    C'**ERROR** WORK-CODE XX IS NOT ON FILE'                         
WCONEMSG DC    C'**ERROR** WORK-CODE XX IS ALREADY ON ESTIMATE'                 
INVCOPYM DC    C'**ERROR** ESTIMATES ALREADY EXIST ON JOB'                      
NDREVMSG DC    C'**ERROR** REVISION NUMBER REQUIRED WITH REVISION DATE'         
NODATMSG DC    C'ENTER WORK-CODES AND ESTIMATES'                                
RECDSMSG DC    C'RECORD DISPLAYED - ENTER CHANGES'                              
RECCHMSG DC    C'RECORD CHANGED -  ENTER FOR NEXT'                              
NOMORMSG DC    C'RECORD CHANGED - ENTER NEXT KEY'                               
NEWESMSG DC    C'**ERROR** JOB USES NEW ESTIMATES'                              
         SPACE 1                                                                
MXLINES  EQU   10                                                               
MXCHRG   EQU   100                                                              
MXWKCNT  EQU   75                                                               
OK       EQU   X'FF'                                                            
STANDX   DS    0CL24                                                            
         DC    X'3C18'                                                          
         DC    PL2'10'                                                          
         DC    PL3'10000'                                                       
         DC    PL4'5000'                                                        
         DC    C'Y'                                                             
         DC    C'N'                                                             
         DC    C'Y'                                                             
         DC    C'N'                                                             
         DC    C' '                                                             
         DC    C' '                                                             
         DC    7X'00'                                                           
NEW      EQU   1                                                                
AMEND    EQU   2                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HANDLE TOTALS FOR JOB                                 
         SPACE 1                                                                
         DS    0D                                                               
JOBTOTS  NMOD1 0,*JOBT*                                                         
         L     RC,0(R1)            LFM WORKING STORAGE                          
         L     R8,4(R1)            LOCAL STORAGE                                
         L     RA,8(R1)            SCREEN                                       
         LA    R7,TWA1             SAVED TWA                                    
         MVC   TOTORIG(18),=3PL6'0'                                             
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         SPACE 1                                                                
JT2      CLI   0(R4),0                                                          
         BE    JT8                                                              
         CLI   0(R4),X'35'                                                      
         BE    JT4                                                              
         CLI   0(R4),X'32'         BALANCE HAS ACTUAL AND BILLS                 
         BNE   JT6                                                              
         USING ACBALD,R4                                                        
         ZAP   TOTACT,ACBLDR                                                    
         ZAP   TOTBILL,ACBLCR                                                   
         B     JT6                                                              
         SPACE 1                                                                
         USING ACESTD,R4                                                        
JT4      AP    TOTORIG,ACESTORG    BUDGETS HAVE ORIGINAL AND CURRENT            
         AP    TOTCURR,ACESTCUR                                                 
         MVI   ESTSW,C'Y'          JOB HAS ESTIMATES                            
         CLI   REV,C'Y'                                                         
         BNE   JT6                                                              
         CLI   ACESTLEN,22                                                      
         BL    JT6                                                              
         AP    TOTPRV,ACESTPRV                                                  
         SPACE 1                                                                
JT6      IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     JT2                                                              
         SPACE 1                                                                
*&&UK                                                                           
JT8      LA    R2,LOGTOT                                                        
         OI    LOGTOTH+6,X'80'                                                  
         MVC   0(6,R2),=C'TOTALS'                                               
         EDIT  (P6,TOTBILL),(11,10(R2)),2,ZERO=BLANK,MINUS=YES                  
         EDIT  (P6,TOTORIG),(11,22(R2)),2,ALIGN=LEFT,MINUS=YES                  
         EDIT  (P6,TOTCURR),(11,45(R2)),2,ALIGN=LEFT,MINUS=YES                  
         EDIT  (P6,TOTINCR),(11,56(R2)),2,ZERO=BLANK,MINUS=YES                  
         EDIT  (P6,TOTACT),(11,67(R2)),2,ZERO=BLANK,MINUS=YES                   
*&&                                                                             
*&&US                                                                           
JT8      MVC   LOGTOT1,SPACES                                                   
         OI    LOGTOT1H+6,X'80'                                                 
         MVC   LOGTOT1(12),=C'*JOB TOTALS*'                                     
         LA    R2,LOGTOT1                                                       
         CP    TOTORIG,=P'0'                                                    
         BE    JT9                                                              
         EDIT  (P6,TOTORIG),(11,22(R2)),2,ALIGN=LEFT,MINUS=YES                  
JT9      CP    TOTPRV,=P'0'                                                     
         BE    JT10                                                             
         EDIT  (P6,TOTPRV),(11,37(R2)),2,ALIGN=LEFT,MINUS=YES                   
JT10     CP    TOTCURR,=P'0'                                                    
         BE    JT11                                                             
         EDIT  (P6,TOTCURR),(11,52(R2)),2,ALIGN=LEFT,MINUS=YES                  
JT11     CP    TOTACT,=P'0'                                                     
         BE    JT12                                                             
         EDIT  (P6,TOTACT),(11,67(R2)),2,ALIGN=LEFT,MINUS=YES                   
         SPACE 1                                                                
JT12     MVC   LOGTOT2,SPACES                                                   
         OI    LOGTOT2H+6,X'80'                                                 
         LA    R2,LOGTOT2                                                       
         CP    TOTBILL,=P'0'                                                    
         BE    JT13                                                             
         MVC   LOGTOT2(12),=C'NET BILLING='                                     
         EDIT  (P6,TOTBILL),(11,12(R2)),2,ALIGN=LEFT,MINUS=YES                  
JT13     CP    TOTINCR,=P'0'                                                    
         BE    JT14                                                             
         MVC   LOGTOT2+42(10),=C'INCREMENT='                                    
         EDIT  (P6,TOTINCR),(11,52(R2)),2,ALIGN=LEFT,MINUS=YES                  
JT14     B     JOBTX                                                            
*&&                                                                             
JOBTX    XMOD1 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
* SUB-ROUTINE TO READ THE OPTIONS WITH GETOPT                                   
*                                                                               
RDOPT    NMOD1 0,**RDOPT*                                                       
         L     RC,0(R1)            RC=LFM WORKING STORAGE                       
         L     R8,4(R1)            R8=LOCAL WORKING STORAGE                     
         L     RA,8(R1)            RA=A(TWA)                                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A84' GET V(GETOPT)                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETOPT,0(R1)                                                     
*                                                                               
RDOPT2   MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL(1),COMPANY                                              
         MVC   GOSELCUL+1(2),36(RA) UNIT/LEDGER                                 
*                                                                               
         LA    R2,LOGCLIH          CLIENT CODE                                  
         GOTO1 MOVE                                                             
         MVC   GOSELCLI,WORK                                                    
*                                                                               
         LA    R2,LOGPRDH          PRODUCT CODE                                 
         GOTO1 MOVE                                                             
         MVC   GOSELPRO,WORK                                                    
*                                                                               
         LA    R2,LOGJOBH          JOB CODE                                     
         GOTO1 MOVE                                                             
         MVC   GOSELJOB,WORK                                                    
*                                                                               
         MVI   GOWHICH,0           BOTH OLD AND NEW OPTIONS                     
         MVI   GOANYMED,0                                                       
*                                                                               
RDOPT4   GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
RDOPTX   XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*              DSECT FOR PROGRAM WORKING STORAGE                                
ESTIMD   DSECT                                                                  
RELO     DS    A                                                                
ACBLTYP  DS    V                                                                
SQUASHER DS    V                                                                
BINSRCH  DS    V                                                                
AJOBTOTS DS    A                                                                
DISOPT   DS    V                                                                
GETOPT   DS    V                                                                
         SPACE 1                                                                
THISACT  DS    CL1                                                              
CHNGSW   DS    CL1                                                              
         SPACE 1                                                                
TOTBILL  DS    PL6                                                              
TOTORIG  DS    PL6                                                              
TOTPRV   DS    PL6                                                              
TOTCURR  DS    PL6                                                              
TOTINCR  DS    PL6                                                              
TOTACT   DS    PL6                                                              
ADDONS   DS    PL6                                                              
ADDFST   DS    CL1                                                              
         SPACE 1                                                                
LOW      DS    CL2                                                              
HI       DS    CL2                                                              
WC       DS    CL2                                                              
WCNME    DS    CL15                                                             
         SPACE 1                                                                
ESTSW    DS    CL1                 ESTIMATES ALREADY ON JOB                     
OFFICE   DS    CL2                                                              
PROFKEY  DS    CL16                                                             
PROFPARA DS    3F                                                               
         SPACE 1                                                                
JOBNAME  DS    CL(L'LOGJOBN)                                                    
         SPACE 1                                                                
*&&US                                                                           
       ++INCLUDE ACDOBLOCK                                                      
         SPACE 2                                                                
       ++INCLUDE ACGOBLOCK                                                      
*&&                                                                             
         SPACE 1                                                                
TWA1     DS    CL2600                                                           
ESTIMX   EQU   *                                                                
         EJECT                                                                  
*              DSECT FOR CHARGES TABLE                                          
CHARD    DSECT                                                                  
CHARCD   DS    CL2                 WORK-CODE                                    
CHARAMT  DS    PL6                 AMOUNT                                       
CHARLNG  EQU   *-CHARD                                                          
         SPACE 2                                                                
*              DSECT FOR TWA1                                                   
TWA1D    DSECT                                                                  
*                                                                               
*                                                                               
WKLST    DS    (2*MXWKCNT)C                                                     
WKCNT    DS    CL1                                                              
         SPACE 1                                                                
CHARGES  DS    (MXCHRG*CHARLNG)C                                                
CHRCNT   DS    CL1                                                              
         SPACE 2                                                                
*              DSECT FOR A SCREEN LINE                                          
SCRD     DSECT                                                                  
SCRWCH   DS    CL8                                                              
SCRWC    DS    CL2                 WORK-CODE                                    
         DS    CL2                                                              
SCRDES   DS    CL15                WORK-CODE DESCRIPTION                        
SCRORGH  DS    CL8                                                              
*&&US                                                                           
SCRORG   DS    CL13                ORIGINAL                                     
SCRPRVH  DS    CL8                                                              
SCRPRV   DS    CL13                PREVIOUS                                     
SCRCURH  DS    CL8                                                              
SCRCUR   DS    CL13                CURRENT                                      
*&&                                                                             
*&&UK                                                                           
SCRORG   DS    CL21                ORIGINAL                                     
SCRCURH  DS    CL8                                                              
SCRCUR   DS    CL21                CURRENT                                      
*&&                                                                             
SCRACTH  DS    CL8                                                              
SCRACT   DS    CL10                ACTUAL                                       
SCRLEN   EQU   *-SCRD                                                           
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMEFD                                                       
SAVELET  DS    CL1                                                              
SAVECPRF DS    CL43                                                             
SAVEPPRF DS    CL43                                                             
SAVEJPRF DS    CL43                                                             
SAVESTAT DS    CL1                                                              
SAVESTA2 DS    CL1                                                              
SAVECXPR DS    CL24                                                             
SAVEPXPR DS    CL24                                                             
SAVEJXPR DS    CL24                                                             
LASTWC   DS    CL1                                                              
JOBKEY   DS    CL32                                                             
LASTACT  DS    CL1                                                              
PROGPROF DS    CL16                                                             
FIRST    DS    CL1                                                              
REV      DS    CL1                                                              
REVN     DS    CL2                                                              
REVD     DS    CL3                                                              
         SPACE 2                                                                
*ACLFMWORK                                                                      
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*ACLFMEQU                                                                       
*ACOPTEQUS - US ONLY                                                            
*DDFLDIND                                                                       
*DDCOMFACS                                                                      
*FAFACTS                                                                        
*FATWA                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
*&&US&&++INCLUDE ACOPTEQUS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACLFM10   05/01/02'                                      
         END                                                                    

*          DATA SET ACLFM0B    AT LEVEL 029 AS OF 05/01/02                      
*PHASE T6030BA,*                                                                
*INCLUDE ACDISOPT                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE BINSRCH                                                                
         TITLE 'ESTIMATING ON-LINE ENQUIRY'                                     
T6030B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ESTIMX-ESTIMD,**LFMB**,R9,RR=R5,CLEAR=YES                        
         LR    R8,RC                                                            
         USING ESTIMD,R8           PROGRAM WORKING STORAGE                      
         LA    R7,TWA1                                                          
         USING TWA1D,R7            SAVED TWA                                    
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)            LFM WORKING STORAGE                          
         USING LOGWORKD,RC                                                      
         ST    R5,RELO                                                          
         L     R2,=V(ACDISOPT)                                                  
         A     R2,RELO                                                          
         ST    R2,DISOPT                                                        
         L     R2,=V(SQUASHER)                                                  
         A     R2,RELO                                                          
         ST    R2,SQUASHER                                                      
         L     R2,=V(BINSRCH)                                                   
         A     R2,RELO                                                          
         ST    R2,BINSRCH                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A84'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETOPT,0(R1)                                                     
         SPACE 1                                                                
         MVC   DMCB+10(2),2(RA)    GET SAVED TWA1                               
         MVC   DMCB+8(2),=X'0100'                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,TWA1                        
         MVI   ERROR,OK                                                         
         ZAP   TOTINCR,=P'0'                                                    
         OI    LOGSERVH+1,X'01'    SERVICE FIELD IS ALWAYS MODIFIED             
         OI    LOGSERVH+6,X'80'                                                 
         MVC   LOGHEAD,SPACES                                                   
         OI    LOGHEADH+6,X'80'                                                 
         MVC   LOGTOTW,SPACES      CLEAR JOB TOTALS                             
         MVC   LOGTOTO,SPACES                                                   
         OI    LOGTOTWH+6,X'80'                                                 
         OI    LOGTOTOH+6,X'80'                                                 
         MVC   LOGPAGW,SPACES      AND PAGE TOTALS                              
         MVC   LOGPAGO,SPACES                                                   
         OI    LOGPAGWH+6,X'80'                                                 
         OI    LOGPAGOH+6,X'80'                                                 
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
         TM    SAVESTA2,X'20'      USE BILLING AMOUNT ON HEADLINE               
         BZ    EM5                 NO                                           
         CLI   LOGHDL+19,C'E'                                                   
         BE    EM5                 ALREADY SET                                  
         MVC   LOGHDL+19(17),=C'EXTERNAL INTERNAL'                              
         OI    LOGHDLH+6,X'80'                                                  
         SPACE 1                                                                
EM5      TM    LOGCLIH+4,X'20'                                                  
         BO    EM8                                                              
         NI    LOGPRDH+4,X'DF'                                                  
         NI    LOGJOBH+4,X'DF'                                                  
         NI    LOGWCH+4,X'DF'                                                   
         MVC   LOGCLIN,SPACES                                                   
         OI    LOGCLINH+6,X'80'                                                 
         MVC   LOGPRDN,SPACES                                                   
         OI    LOGPRDNH+6,X'80'                                                 
         MVC   LOGJOBN,SPACES                                                   
         OI    LOGJOBNH+6,X'80'                                                 
         SPACE 1                                                                
EM8      GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),36(RA)                                                  
         MVC   KEY+3(6),WORK                                                    
         TM    LOGCLIH+4,X'20'                                                  
         BO    EM12                                                             
         SPACE 1                                                                
         GOTO1 READ                                                             
         MVC   CLIKEY,KEY                                                       
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
         BO    EM16                                                             
         NI    LOGJOBH+4,X'DF'                                                  
         NI    LOGWCH+4,X'DF'                                                   
         MVC   LOGPRDN,SPACES                                                   
         OI    LOGPRDNH+6,X'80'                                                 
         MVC   LOGJOBN,SPACES                                                   
         OI    LOGJOBNH+6,X'80'                                                 
         SPACE 1                                                                
EM16     GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         SR    R3,R3                                                            
         IC    R3,38(RA)                                                        
         LA    R3,KEY+3(R3)                                                     
         MVC   0(6,R3),WORK                                                     
         TM    LOGPRDH+4,X'20'                                                  
         BO    EM20                                                             
         SPACE 1                                                                
         GOTO1 READ                                                             
         MVC   PRODKEY,KEY                                                      
         XC    SAVEPXPR,SAVEPXPR                                                
         LA    R3,SAVEPPRF                                                      
         LA    R5,SAVEPXPR                                                      
         BAS   RE,ANYPROF                                                       
         GOTO1 NAMOUT                                                           
         OI    LOGPRDH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         EJECT                                                                  
*              BUILD KEY (JOB ROUTINES)                                         
         SPACE 1                                                                
EM20     LA    R2,LOGJOBH                                                       
         TM    LOGJOBH+4,X'20'                                                  
         BO    EM22                                                             
         NI    LOGWCH+4,X'DF'                                                   
         MVC   LOGJOBN,SPACES                                                   
         OI    LOGJOBNH+6,X'80'                                                 
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
         SPACE 1                                                                
         GOTO1 GETEL,DMCB,('ACJBELQ',IO),0                                      
         CLI   12(R1),0            TEST JOB ELEMENT FOUND                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
         USING ACJOBD,R4                                                        
         TM    ACJBSTAT,ACJBNEWQ   TEST JOB USES NEW ESTIMATES                  
         BO    NEWESTS             YES-HANDLE AS ERROR                          
         DROP  R4                                                               
         SPACE 1                                                                
EM23     XC    SAVEJXPR,SAVEJXPR                                                
         LA    R3,SAVEJPRF                                                      
         LA    R5,SAVEJXPR                                                      
         BAS   RE,ANYPROF                                                       
         GOTO1 NAMOUT                                                           
         SPACE 1                                                                
EM24     BAS   RE,INGO             INITIALIZE GOBLOCK                           
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   WORK,SPACES                                                      
         MVC   WORK(36),LOGJOBN                                                 
         MVC   WORK+37(5),=C'BILL='                                             
         MVI   DOOPTNUM,OPNBT      DISPLAY BILLING TYPE                         
         LA    RE,GOBLOCK                                                       
         ST    RE,DOAGOBLK         USE GOBLOCK VALUE                            
         LA    RE,WORK+42-8                                                     
         ST    RE,DOAFLDH                                                       
         MVC   DOACOM,COMFACS                                                   
         GOTO1 DISOPT,DMCB,DOBLOCK                                              
         SPACE 1                                                                
EM31     MVC   LOGJOBN,WORK                                                     
         OI    LOGJOBNH+6,X'80'                                                 
         OI    LOGJOBH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         BAS   RE,ESTIN            PUT ESTIMATES TO TABLE                       
         BAS   RE,ACTIN            ADD ACTUAL                                   
         BAS   RE,RATES            GET COMMISSION RATES                         
         EJECT                                                                  
*              BUILD KEY (WORK-CODES)                                           
         SPACE 1                                                                
EM33     LA    R2,LOGWCH                                                        
         TM    LOGWCH+4,X'20'                                                   
         BO    EM34                                                             
         MVI   ANYKEY,C'Y'                                                      
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
         BAS   RE,VALWC            VALIDATE WORK-CODE LINE                      
         CLI   ERROR,OK                                                         
         BNE   EM34                                                             
         OI    LOGWCH+4,X'20'                                                   
EM34     BAS   RE,TWAWRT           SAVE TWA                                     
         B     ERRXIT                                                           
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 1                                                                
EM40     CLI   MODE,DSPLYREC                                                    
         BNE   XIT                                                              
         USING SCRD,R6                                                          
EM50     BAS   RE,CLRSCR           CLEAR SCREEN                                 
         LA    R5,PAGTOT                                                        
         USING CHARD,R5                                                         
         XC    CHARD(CHARLNG),CHARD   CLEAR PAGE ACCUMULATORS                   
         CLI   LASTWC,X'FF'        WAS LIST FINISHED LAST TIME                  
         BNE   EM51                                                             
         MVI   LASTWC,0            IF IT WAS START AGAIN                        
EM51     ZIC   R1,LASTWC                                                        
         STC   R1,FRSTWC           SAVE THE FIRST                               
         LA    R6,LOGFSWCH                                                      
         LA    R3,MXLINES                                                       
         ZIC   R2,WKCNT            TOTAL NUMBER OF WORK CODES                   
EM52     CR    R1,R2                                                            
         BNE   EM53                                                             
         MVI   LASTWC,X'FF'        END OF WORK CODE LIST                        
         B     EM59                                                             
EM53     SLL   R1,1                                                             
         LA    R4,WKLST(R1)                                                     
         MVC   WC,0(R4)            CODE FOR THIS LINE                           
         SRL   R1,1                                                             
         AH    R1,=H'1'                                                         
         STC   R1,LASTWC                                                        
         BAS   RE,PUTSCR           PUT OUT THE LINE                             
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,EM52             MAX OF 10 LINES                              
         CLC   LASTWC,WKCNT                                                     
         BNE   EM59                                                             
         MVI   LASTWC,X'FF'        END OF SCREEN AND LIST                       
         SPACE 1                                                                
EM59     LA    R6,LOGPAGWH                                                      
         MVC   SCRDES,=CL15'**PAGE TOTALS**'                                    
         LA    R5,PAGTOT                                                        
         MVI   WC,X'FF'                                                         
         BAS   RE,PUTSCR                                                        
         LA    R6,LOGTOTWH                                                      
         MVC   SCRDES,=CL15'**JOB TOTALS**'                                     
         LA    R5,JOBTOT                                                        
         MVI   WC,X'FF'                                                         
         BAS   RE,PUTSCR                                                        
         CLI   LASTWC,X'FF'                                                     
         BE    NOMORE              NO MORE DATA TO DISPLAY                      
         B     RECDIS              RECORD DISPLAYED                             
         EJECT                                                                  
*              ROUTINE TO VALIDATE WORK-CODE LINE                               
         SPACE 1                                                                
VALWC    NTR1                                                                   
         SR    R3,R3                                                            
         XC    WKLST,WKLST                                                      
         MVI   WKCNT,0                                                          
         MVI   FRSTWC,0                                                         
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
         CLI   WKCNT,0                                                          
         BNE   XIT                                                              
         B     NODATA                                                           
         EJECT                                                                  
*              ROUTINE TO BUILD LIST OF VALID CODES                             
         SPACE 1                                                                
BLDLST   NTR1                                                                   
         LA    R2,LOGWCH                                                        
         BAS   RE,WCNAME           DOES CODE EXIST                              
         CLC   WCNME,SPACES                                                     
         BE    WCNOTFD             WORK CODE NOT FOUND                          
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
         USING CHARD,R5                                                         
         USING SCRD,R6                                                          
PUTSCR   NTR1                                                                   
         CLI   WC,X'FF'                                                         
         BE    PUTSCR3             DO A TOTAL LINE                              
         MVC   SCRWC,WC                                                         
         BAS   RE,WCNAME           GET W/C NAME                                 
         CLC   WCNME,SPACES                                                     
         BNE   *+10                                                             
         MVC   WCNME,=C'** UNKNOWN **'                                          
         MVC   SCRDES,WCNME                                                     
         LA    R5,CHARGES                                                       
         ZIC   R0,CHRCNT                                                        
PUTSCR2  CLC   CHARCD,WC           FIND TABLE ENRTY FOR THIS CODE               
         BE    PUTSCR3                                                          
         LA    R5,CHARLNG(R5)                                                   
         BCT   R0,PUTSCR2                                                       
         B     XIT                                                              
PUTSCR3  L     R2,CHARORG                                                       
         LA    R4,SCRORG                                                        
         BAS   RE,EDIT             ORIGINAL TO SCREEN                           
         L     R2,CHARCUR                                                       
         LA    R4,SCRCUR                                                        
         BAS   RE,EDIT             CURRENT TO SCREEN                            
         L     R2,CHARACT                                                       
         LA    R4,SCRACT                                                        
         BAS   RE,EDIT             ACTUAL TO SCREEN                             
         L     R2,CHARCUR                                                       
         S     R2,CHARACT          CURRENT LESS ACTUAL                          
         LA    R4,SCRBAL                                                        
         BAS   RE,EDIT             BALANCE TO SCREEN                            
         L     R1,CHARCUR                                                       
         L     R2,CHARACT                                                       
         LTR   R1,R1                                                            
         BZ    PUTSCR5                                                          
         LTR   R2,R2                                                            
         BZ    PUTSCR5                                                          
         LA    R3,100            PERCENT OF ACTUAL VS CURRENT ESTIMATE          
         MR    R2,R2                                                            
         DR    R2,R1                                                            
         LA    R4,SCRESTP                                                       
         EDIT  (R3),(3,1(R4))                                                   
         CH    R3,=H'999'                                                       
         BNH   PUTSCR5                                                          
         MVC   SCRESTP,=C'HIGH'                                                 
PUTSCR5  L     R2,CHARCOM                                                       
         LA    R4,SCRCOMM                                                       
         BAS   RE,EDIT             COMMISSION TO SCREEN                         
         A     R2,CHARCUR           ADD CURRENT TO COMMISSION                   
         LA    R4,SCRGROSS                                                      
         BAS   RE,EDIT             GROSS TO SCREEN                              
         SPACE 1                                                                
         LA    R2,PAGTOT                                                        
         LA    R2,CHARORG-CHARD(R2)                                             
         LA    R1,CHARORG                                                       
         LA    R3,4                                                             
PUTSCR7  L     RE,0(R2)            ADD THIS LINE TO PAGE TOTAL                  
         A     RE,0(R1)                                                         
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,PUTSCR7                                                       
         B     XIT                                                              
         SPACE 1                                                                
EDIT     EDIT  (R2),(10,DMCB),FLOAT=-                                           
         MVC   0(8,R4),DMCB                                                     
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO ADD ESTIMATE TO TABLE                                 
         SPACE 1                                                                
         USING CHARD,R5                                                         
ESTIN    NTR1                                                                   
         LA    R5,CHARGES          CLEAR ESTIMATE/CHARGES TABLE                 
         LA    R3,MXCHRG                                                        
ESTIN1   XC    CHARCD(CHARLNG),CHARCD                                           
         ZAP   CHARATE,=P'1765'    DEFAULT COMMISSION RATE                      
         MVI   CHARSTAT,X'00'                                                   
         LA    R5,CHARLNG(R5)                                                   
         BCT   R3,ESTIN1                                                        
         XR    R0,R0                                                            
         LA    R5,CHARGES                                                       
         GOTO1 GETEL,DMCB,(X'35',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   ESTINX                                                           
         L     R4,DMCB+12                                                       
         USING ACESTD,R4                                                        
ESTIN3   MVC   CHARCD,ACESTWRK     WORK-CODE                                    
         ZAP   DUB,ACESTORG        ORININAL                                     
         CVB   R1,DUB                                                           
         ST    R1,CHARORG                                                       
         ZAP   DUB,ACESTCUR        CURRENT                                      
         CVB   R1,DUB                                                           
         ST    R1,CHARCUR          TO ESTIMATE / CHARGES TABLE                  
         LA    R5,CHARLNG(R5)      NEXT TABLE ENTRY                             
         AH    R0,=H'1'                                                         
ESTIN5   ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    ESTINX              END OF ESTIMATE ELEMENTS                     
         CLI   0(R4),X'35'                                                      
         BE    ESTIN3              NEXT ESTIMATE ELEMENT                        
         B     ESTIN5                                                           
ESTINX   STC   R0,CHRCNT           COUNT NUMBER OF ENTRIES                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD TABLE OF ACTUAL CHARGES                         
         SPACE 1                                                                
         USING CHARD,R5                                                         
ACTIN    NTR1                                                                   
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
         ZAP   DUB,TRNSAMNT                                                     
         CVB   R1,DUB                                                           
         LA    R5,CHARGES                                                       
         ZIC   R3,CHRCNT                                                        
         LTR   R3,R3                                                            
         BZ    ACTIN6              ADD FIRST ONE                                
ACTIN5   CLC   IO+15(2),CHARCD          WORK-CODE MATCH                         
         BE    ACTIN9                                                           
         LA    R5,CHARLNG(R5)                                                   
         BCT   R3,ACTIN5                                                        
         ZIC   R3,CHRCNT                                                        
         LA    R0,MXCHRG                                                        
         CR    R3,R0                    ALL READY HAVE MAX                      
         BE    ACTIN9                   MAKE IT   OTHERS                        
ACTIN6   MVC   CHARCD,IO+15             WORK CODE TO TABLE                      
         ZIC   R3,CHRCNT                                                        
         AH    R3,=H'1'                                                         
         STC   R3,CHRCNT                UPDATE COUNT                            
         SPACE 1                                                                
ACTIN9   L     R2,CHARACT                                                       
         AR    R2,R1                                                            
         ST    R2,CHARACT          UPDTAE ACTUAL                                
         B     ACTIN3                                                           
         SPACE 1                                                                
ACTIN11  MVC   KEY,KEYSAVE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD COMMISSION RATE AND COMMISSION                    
         SPACE 1                                                                
* GET COMMISSION RATE THE NEW WAY                                               
*                                                                               
RATES    NTR1                                                                   
         BAS   RE,INGO                                                          
         LA    R5,CHARGES                                                       
         ZIC   R0,CHRCNT                                                        
         LTR   R0,R0                                                            
         BZ    XIT                                                              
RATES2   MVC   GOSELWC,CHARCD                                                   
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         ZAP   CHARATE,GOAGYCOM                                                 
         LA    R5,CHARLNG(R5)                                                   
         BCT   R0,RATES2                                                        
         SPACE 1                                                                
         LA    R5,JOBTOT                                                        
         XC    CHARCD(CHARLNG),CHARCD  CLEAR JOB TOTAL LINE                     
         LA    R5,CHARGES          WORK-OUT COMMISSION                          
         ZIC   R0,CHRCNT                                                        
         LTR   R0,R0                                                            
         BZ    XIT                                                              
RATES3   L     R3,CHARCUR         CURRENT CHARGES                               
         CVD   R3,DUB                                                           
         ZAP   PL13,DUB                                                         
         MP    PL13,CHARATE        X RATE                                       
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   DUB,PL13                                                         
         CVB   R3,DUB                                                           
         ST    R3,CHARCOM          SAVE COMMISSION                              
         LA    R2,JOBTOT                                                        
         LA    R2,CHARORG-CHARD(R2)      ADD ACCUMS TO TOTAL LINE               
         LA    R1,CHARORG                                                       
         LA    R3,4                                                             
RATES5   L     RE,0(R2)                                                         
         A     RE,0(R1)                                                         
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,RATES5                                                        
         LA    R5,CHARLNG(R5)                                                   
         BCT   R0,RATES3                                                        
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE GOBLOCK                                             
*                                                                               
INGO     MVC   GOADM,DATAMGR                                                    
         XC    GOACLI,GOACLI                                                    
         XC    GOAPRO,GOAPRO                                                    
         XC    GOAJOB,GOAJOB                                                    
         XC    GOACOMP,GOACOMP                                                  
         MVC   GOSELCUL,JOBKEY                                                  
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
         MVC   GOSELCLI(3),CLIKEY+3                                             
         MVC   GOSELPRO,PRODKEY+6                                               
         MVC   GOSELJOB,JOBKEY+9                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO CLEAR  SCREEN                                         
*              R3 NUMBER OF LINES REMAINING                                     
*              R6 A(FIRST HEADER ON LINE)                                       
         SPACE 1                                                                
         USING SCRD,R6                                                          
CLRSCR   NTR1                                                                   
         LA    R3,MXLINES                                                       
         LA    R6,LOGFSWCH                                                      
CLRSCR1  TWAXC SCRWCH,SCRH,PROT=Y                                               
         LA    R6,SCRLEN(R6)                                                    
         BCT   R3,CLRSCR1                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE WORK-CODE                                    
         SPACE 1                                                                
WCNAME   NTR1                                                                   
         MVC   WCNME,SPACES                                                     
         MVC   KEY,SPACES          GO AND VALIDATE WORK CODE                    
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),36(RA)                                                  
         MVC   KEY+4(2),WC                                                      
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE                                                      
         BNE   XIT                                                              
         GOTO1 GETEL,DMCB,(X'12',IO),0                                          
         CLI   DMCB+12,0                                                        
         BNE   XIT                                                              
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
*              ERROR HANDLING ROUTINES                                          
         SPACE 1                                                                
INVINP   MVI   ERROR,2                                                          
         B     CURSOR                                                           
INVFLD   MVC   LOGHEAD(L'INVFMSG),INVFMSG                                       
         LA    R4,LOGHEAD+L'INVFMSG                                             
INVF2    EDIT  (R3),(2,0(R4)),ALIGN=LEFT                                        
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGWCH                                                        
         B     CURSOR                                                           
         SPACE 1                                                                
INVDUP   MVC   LOGHEAD(L'INVDMSG),INVDMSG                                       
         LA    R4,LOGHEAD+L'INVDMSG                                             
         LA    R2,LOGWCH                                                        
         B     INVF2                                                            
         SPACE 1                                                                
ESTNOTFD MVC   LOGHEAD(L'ESTNOMSG),ESTNOMSG                                     
         MVC   LOGHEAD+20(2),WC                                                 
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
WCNOTFD  MVC   LOGHEAD(L'WCNOTMSG),WCNOTMSG                                     
         MVC   LOGHEAD+20(2),WC                                                 
         MVI   ERROR,X'FE'                                                      
         B     CURSOR                                                           
         SPACE 1                                                                
NODATA   MVC   LOGHEAD(L'NODATMSG),NODATMSG                                     
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGENDH                                                       
         B     CURSOR                                                           
NOINPUT  MVI   ERROR,1                                                          
         B     CURSOR                                                           
         SPACE 1                                                                
RECDIS   MVC   LOGHEAD(L'RECDSMSG),RECDSMSG                                     
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGENDH                                                       
         B     CURSOR                                                           
         SPACE 1                                                                
NOMORE   MVC   LOGHEAD(L'NOMORMSG),NOMORMSG                                     
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGCLIH                                                       
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
ESTNOMSG DC    C'**ERROR** WORK-CODE XX NOT ON ESTIMATE'                        
WCNOTMSG DC    C'**ERROR** WORK-CODE XX IS NOT ON FILE'                         
NODATMSG DC    C'**ERROR** NO DATA TO DISPLAY'                                  
NEWESMSG DC    C'**ERROR** JOB USES NEW ESTIMATES'                              
RECDSMSG DC    C'RECORD DISPLAYED - HIT ENTER FOR NEXT'                         
NOMORMSG DC    C'NO MORE DATA TO DISPLAY - ENTER NEXT KEY'                      
         SPACE 1                                                                
MXLINES  EQU   10                                                               
MXCHRG   EQU   100                                                              
MXWKCNT  EQU   75                                                               
OK       EQU   X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR PROGRAM WORKING STORAGE                                
ESTIMD   DSECT                                                                  
RELO     DS    A                                                                
SQUASHER DS    V                                                                
BINSRCH  DS    V                                                                
GETOPT   DS    V                                                                
DISOPT   DS    V                                                                
         SPACE 1                                                                
TOTBILL  DS    PL6                                                              
TOTORIG  DS    PL6                                                              
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
PL13     DS    PL13                                                             
GOBUFF   DS    CL100                                                            
       ++INCLUDE ACDOBLOCK                                                      
       ++INCLUDE ACGOBLOCK                                                      
         SPACE 1                                                                
TWA1     DS    CL6144                                                           
ESTIMX   EQU   *                                                                
         EJECT                                                                  
*              DSECT FOR CHARGES AND ESTIMATE TABLE                             
CHARD    DSECT                                                                  
CHARCD   DS    CL2                 WORK-CODE                                    
CHARSTAT DS    CL1                 RATE STATUS X'80' 4 DP                       
         DS    CL1                 SPARE                                        
CHARATE  DS    PL4                 COMMISSION RATE                              
CHARORG  DS    CL4                 ORIGINAL                                     
CHARCUR  DS    CL4                 CURRENT                                      
CHARCOM  DS    CL4                 COMMISSION                                   
CHARACT  DS    CL4                 ACTUAL                                       
CHARLNG  EQU   *-CHARD                                                          
         SPACE 2                                                                
*              DSECT FOR TWA1                                                   
TWA1D    DSECT                                                                  
WKCNT    DS    CL1                                                              
CHRCNT   DS    CL1                                                              
WKLST    DS    (2*MXWKCNT)C                                                     
         DS    0F                                                               
CHARGES  DS    (MXCHRG*CHARLNG)C                                                
PAGTOT   DS    (CHARLNG)C          PAGE TOTALS                                  
JOBTOT   DS    (CHARLNG)C          JOB TOTALS                                   
         SPACE 2                                                                
*              DSECT FOR A SCREEN LINE                                          
SCRD     DSECT                                                                  
SCRWCH   DS    CL8                                                              
SCRWC    DS    CL2                 WORK-CODE                                    
         DS    CL1                                                              
SCRDES   DS    CL15                WORK-CODE DESCRIPTION                        
SCRH     DS    CL8                                                              
SCRORG   DS    CL8                 ORIGINAL                                     
         DS    CL1                                                              
SCRCUR   DS    CL8                 CURRENT                                      
         DS    CL1                                                              
SCRACT   DS    CL8                 ACTUAL                                       
         DS    CL1                                                              
SCRBAL   DS    CL8                 BALANCE                                      
         DS    CL1                                                              
SCRESTP  DS    CL4                 PERCENT ESTIMATE                             
         DS    CL1                                                              
SCRCOMM  DS    CL8                 COMMISSION                                   
         DS    CL1                                                              
SCRGROSS DS    CL8                 GROSS                                        
SCRLEN   EQU   *-SCRD                                                           
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF4D                                                       
SAVELET  DS    CL1                                                              
SAVECPRF DS    CL42                                                             
SAVEPPRF DS    CL42                                                             
SAVEJPRF DS    CL42                                                             
SAVESTAT DS    CL1                                                              
SAVESTA2 DS    CL1                                                              
SAVECXPR DS    CL24                                                             
SAVEPXPR DS    CL24                                                             
SAVEJXPR DS    CL24                                                             
FRSTWC   DS    CL1                                                              
LASTWC   DS    CL1                                                              
CLIKEY   DS    CL32                                                             
PRODKEY  DS    CL32                                                             
JOBKEY   DS    CL32                                                             
         SPACE 2                                                                
*ACLFMWORK                                                                      
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*ACLFMEQU                                                                       
*ACOPTEQUS                                                                      
*DDFLDIND                                                                       
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE ACOPTEQUS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACLFM0B   05/01/02'                                      
         END                                                                    

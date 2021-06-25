*          DATA SET ACPRO21    AT LEVEL 006 AS OF 04/10/15                      
*PHASE T60B21A                                                                  
         TITLE 'T60B21-  PRODUCTION RULES REPORT'                               
T60B21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDX-MYD),*T60B21*                                              
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    LASTSORT,LASTSORT                                                
         B     XIT                                                              
         EJECT                                                                  
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,READRECS                                                      
         B     XIT                                                              
         EJECT                                                                  
VREC     NTR1                                                                   
         MVI   OPTION,C'Y'                                                      
         LA    R2,CONRECH                                                       
*                                                                               
         MVC   CLICODE,SPACES                                                   
         MVC   PRODCODE,SPACES                                                  
         MVC   JOBNUM,SPACES                                                    
*                                                                               
         GOTO1 SETHEIR                                                          
*                                                                               
         LA    R2,PROOGRH          OFFICE GROUP IS OPTIONAL                     
         XC    REQOGR,REQOGR                                                    
         CLI   5(R2),0                                                          
         BE    VREC020                                                          
         GOTO1 VALOG                                                            
         MVC   REQOGR,8(R2)                                                     
*                                                                               
VREC020  LA    R2,PROOFFH          OFFICE IS OPTIONAL                           
         XC    REQOFF,REQOFF                                                    
         CLI   5(R2),0                                                          
         BE    VREC060                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 ANY                                                              
         MVI   NEGSW,C'N'                                                       
         CLI   8(R2),C'*'          TEST FOR NEGATIVE FILTER                     
         BNE   VREC040                                                          
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                DECREMENT INPUT LENGTH                       
         STC   R1,5(R2)                                                         
         MVC   PROOFF,WORK+1       SHIFT INPUT OVER 1 BYTE                      
*                                                                               
VREC040  GOTO1 VALOFF                                                           
         MVC   REQOFF,EFFOFFC      SAVE THE OFFICE                              
         CLI   NEGSW,C'Y'                                                       
         BNE   *+8                                                              
         NI    REQOFF,X'FF'-X'40'                                               
*                                                                               
VREC060  LA    R2,PROCLIH          CLIENT IS OPTIONAL                           
         MVC   CLICODE,=CL6'A'                                                  
         MVI   EXLEN,2                                                          
         CLI   5(R2),0                                                          
         BE    VREC080                                                          
*                                                                               
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,LCLI                                                          
         LA    R1,2(R1)                                                         
         STC   R1,EXLEN                                                         
         OI    6(R2),X'80'                                                      
         GOTO1 VALCLI                                                           
*                                                                               
VREC080  LA    R2,PROPROH          PRODUCT IS OPTIONAL                          
         CLI   5(R2),0                                                          
         BE    VREC100                                                          
         MVI   ERROR,NEEDCLI                                                    
         CLI   PROCLIH+5,0                                                      
         BE    ERREND                                                           
         SR    R1,R1                                                            
         IC    R1,LCLIPRO                                                       
         LA    R1,2(R1)                                                         
         STC   R1,EXLEN                                                         
         OI    6(R2),X'80'                                                      
         GOTO1 VALPROD                                                          
*                                                                               
VREC100  LA    R2,PROJOBH          JOB IS OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VREC120                                                          
         MVI   ERROR,NEEDPRO                                                    
         CLI   PROPROH+5,0                                                      
         BE    ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH MEDIA                    
         CLI   PROMEDH+5,0                                                      
         BNE   ERREND                                                           
         CLI   PROMGRH+5,0         NOT COMPATIBLE WITH MEDIA GROUP              
         BNE   ERREND                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,LCLIJOB                                                       
         LA    R1,2(R1)                                                         
         STC   R1,EXLEN                                                         
         OI    6(R2),X'80'                                                      
         GOTO1 VALJOB                                                           
*                                                                               
VREC120  LA    R2,PROMGRH          MEDIA GROUP                                  
         XC    REQMGR,REQMGR                                                    
         CLI   5(R2),0                                                          
         BE    VREC140                                                          
         GOTO1 VALMG                                                            
         MVC   REQMGR,8(R2)                                                     
*                                                                               
VREC140  LA    R2,PROMEDH          MEDIA                                        
         XC    REQMED,REQMED                                                    
         CLI   5(R2),0                                                          
         BE    VRECX                                                            
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   REQMED,8(R2)                                                     
*                                                                               
VRECX    B     XIT                                                              
         EJECT                                                                  
READRECS NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),JOBNUM                                                   
*                                                                               
READ020  BAS   RE,READHI                                                        
*                                                                               
READ040  ZIC   R1,EXLEN            CHECK C/B ON CLIENT/PRODUCT                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   GETSORT             INPUT FINISHED, GET RECORDS                  
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         CLI   0(R1),C' '          IS THIS A CLIENT ?                           
         BH    READ060             NO                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVEKEY,KEY                                                      
         GOTO1 SETCLI                                                           
         GOTO1 SETOG                                                            
*                                                                               
         BAS   RE,CHKOGR           YES, CORRECT OFFICE GROUP ?                  
         BNE   CLISKIP             NO, SKIP IT                                  
         BAS   RE,CHKOFF           YES, CORRECT OFFICE ?                        
         BNE   CLISKIP             NO, SKIP IT                                  
         BAS   RE,CLISORT          YES, SEND IT TO SORT                         
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,REREAD                                                        
         BAS   RE,READNXT                                                       
         B     READ040                                                          
*                                                                               
READ060  ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         CLI   0(R1),C' '          IS THIS A PRODUCT ?                          
         BH    READ080             NO                                           
         MVC   SAVEKEY,KEY                                                      
         GOTO1 SETPROD                                                          
         GOTO1 SETOG                                                            
*                                                                               
         BAS   RE,CHKOGR           YES, CORRECT OFFICE GROUP ?                  
         BNE   PROSKIP             NO, SKIP IT                                  
         BAS   RE,CHKOFF           YES, CORRECT OFFICE ?                        
         BNE   PROSKIP             NO SKIP IT                                   
         BAS   RE,PROSORT          YES, SEND IT TO SORT                         
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,REREAD                                                        
         BAS   RE,READNXT                                                       
         B     READ040                                                          
*                                                                               
READ080  BAS   RE,CHKOGR           IS OFFICE GROUP CORRECT ?                    
         BNE   JOBSKIP             NO, SKIP IT                                  
         BAS   RE,CHKOFF           YES, IS OFFICE CORRECT ?                     
         BNE   JOBSKIP             NO, SKIP IT                                  
*                                                                               
READ100  GOTO1 SETJOB              YES, SET MEDIA AND MEDIA GROUP               
         MVC   SAVEKEY,KEY         SAVE THE KEY                                 
         MVC   MEDIA,JOBNUM                                                     
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(1),MEDIA                                                   
         GOTO1 HIGH                                                             
         MVI   ERROR,BADMEDIA                                                   
         CLC   KEY(3),KEYSAVE                                                   
         BNE   ERREND                                                           
         GOTO1 SETMED                                                           
*                                                                               
         BAS   RE,CHKMGR           IS MEDIA GROUP CORRECT ?                     
         BNE   JOBSKIP             NO, SKIP IT                                  
         BAS   RE,CHKMED           YES, IS MEDIA CORRECT ?                      
         BNE   JOBSKIP             NO, SKIP IT                                  
         BAS   RE,JOBSORT          YES, SEND IT TO SORT                         
*                                                                               
JOBSKIP  MVC   KEY,SAVEKEY         YES, RESTORE THE KEY                         
         BAS   RE,REREAD           RE-READ THE JOB RECORD                       
         ZIC   R1,LCLIJOB                                                       
         LA    R1,KEY+3-1(R1)                                                   
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         B     READ020             GET NEXT JOB                                 
*                                                                               
PROSKIP  MVC   KEY,SAVEKEY         YES, RESTORE THE KEY                         
         BAS   RE,REREAD           RE-READ THE PRODUCT RECORD                   
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3-1(R1)                                                   
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         B     READ020             GET NEXT PRODUCT                             
*                                                                               
CLISKIP  MVC   KEY,SAVEKEY         YES, RESTORE THE KEY                         
         BAS   RE,REREAD           RE-READ THE CLIENT RECORD                    
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3-1(R1)                                                   
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         B     READ020             GET NEXT CLIENT                              
         EJECT                                                                  
CLISORT  ST    RE,SAVERE                                                        
         XC    SORTREC,SORTREC                                                  
         MVC   SORTCLI,CLICODE                                                  
         MVC   SORTOG,EFFOFG                                                    
         MVC   SORTOFF,EFFOFFC                                                  
         BAS   RE,PUTSORT                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
PROSORT  ST    RE,SAVERE                                                        
         XC    SORTREC,SORTREC                                                  
         MVC   SORTCLI,CLICODE                                                  
         MVC   SORTPRO,PRODCODE                                                 
         MVC   SORTOG,EFFOFG                                                    
         MVC   SORTOFF,EFFOFFC                                                  
         BAS   RE,PUTSORT                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
JOBSORT  ST    RE,SAVERE                                                        
         XC    SORTREC,SORTREC                                                  
         MVC   SORTCLI,CLICODE                                                  
         MVC   SORTPRO,PRODCODE                                                 
         MVC   SORTJOB,JOBNUM                                                   
         MVC   SORTOG,EFFOFG                                                    
         MVC   SORTOFF,EFFOFFC                                                  
         MVC   SORTMG,MGROUP                                                    
         MVC   SORTMED,MEDIA                                                    
         BAS   RE,PUTSORT                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
CHKOFF   OC    REQOFF,REQOFF                                                    
         BZ    CHKOFFY                                                          
         TM    REQOFF,X'40'        TEST UPPER CASE BIT ON                       
         BZ    CHKOFF2             NO-DO NEGATIVE TEST                          
*                                                                               
         CLC   REQOFF,EFFOFFC                                                   
         BE    CHKOFFY                                                          
         B     CHKOFFN                                                          
*                                                                               
CHKOFF2  MVC   HALF,REQOFF                                                      
         OI    HALF,X'40'                                                       
         CLC   HALF,EFFOFFC                                                     
         BE    CHKOFFN                                                          
         B     CHKOFFY                                                          
*                                                                               
CHKOFFN  LTR   RB,RB                                                            
         B     CHKOFFX                                                          
*                                                                               
CHKOFFY  CR    RB,RB                                                            
*                                                                               
CHKOFFX  BR    RE                                                               
         SPACE 3                                                                
CHKOGR   OC    REQOGR,REQOGR                                                    
         BZ    CHKOFFY                                                          
         CLC   REQOGR,EFFOFG                                                    
         BE    CHKOFFY                                                          
         B     CHKOFFN                                                          
         SPACE 3                                                                
CHKMGR   OC    REQMGR,REQMGR                                                    
         BZ    CHKOFFY                                                          
         CLC   REQMGR,MGROUP                                                    
         BE    CHKOFFY                                                          
         B     CHKOFFN                                                          
         SPACE 3                                                                
CHKMED   OC    REQMED,REQMED                                                    
         BZ    CHKOFFY                                                          
         CLC   REQMED,MEDIA                                                     
         BE    CHKOFFY                                                          
         B     CHKOFFN                                                          
         EJECT                                                                  
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     XIT                                                              
         SPACE 3                                                                
GETSORT  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R6,15,DMCB+4                                                     
         BZ    DONE                                                             
         MVC   SORTREC,0(R6)                                                    
         BAS   RE,PRNTREP          PRINT REPORT                                 
         B     GETSORT                                                          
         SPACE 3                                                                
DONE     GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
PRNTREP  ST    RE,SAVERE                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
         OC    SORTOG,SORTOG       OFFICE GROUP ?                               
         BZ    PRNT020             NO                                           
         CLC   LASTOG,SORTOG       YES, SAME AS LAST ?                          
         MVC   LASTOG,SORTOG                                                    
         BE    PRNT020             YES                                          
         XC    LASTOFF(LASTMED-LASTOFF+1),LASTOFF                               
         BAS   RE,SETGO            SETUP GOBLOCK                                
         MVC   GOSELOG,SORTOG                                                   
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   POG,SORTOG                                                       
         BAS   RE,PRINT                                                         
*                                                                               
PRNT020  OC    SORTOFF,SORTOFF     OFFICE CODE ?                                
         BZ    PRNT040             NO                                           
         CLC   LASTOFF,SORTOFF     YES, SAME AS LAST ?                          
         MVC   LASTOFF,SORTOFF                                                  
         BE    PRNT040             YES                                          
         XC    LASTCLI(LASTMED-LASTCLI+1),LASTCLI                               
         BAS   RE,SETGO            SETUP GOBLOCK                                
         MVC   GOSELOFC,SORTOFF                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   POFF,SORTOFF                                                     
         BAS   RE,PRINT                                                         
*                                                                               
PRNT040  OC    SORTPRO,SORTPRO     IS THIS A CLIENT ONLY ?                      
         BNZ   PRNT060             NO, HANDLE LIKE A PRODUCT                    
         CLC   LASTCLI,SORTCLI     YES, SAME CLIENT AS BEFORE ?                 
         MVC   LASTCLI,SORTCLI                                                  
         BE    PRNT060             YES                                          
         XC    LASTPRO(LASTMED-LASTPRO+1),LASTPRO                               
         BAS   RE,SETGO            SETUP GOBLOCK                                
         MVC   GOSELCLI,SORTCLI                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   PCLI,SORTCLI                                                     
         BAS   RE,PRINT                                                         
*                                                                               
PRNT060  OC    SORTPRO,SORTPRO     PRODUCT ?                                    
         BZ    PRNTX               NO, DONE                                     
         OC    SORTJOB,SORTJOB     IS THIS PRODUCT ONLY ?                       
         BNZ   PRNT080             NO, HANDLE LIKE A JOB                        
         CLC   LASTPRO,SORTPRO     YES, SAME PRODUCT AS BEFORE ?                
         MVC   LASTPRO,SORTPRO                                                  
         BE    PRNT080             YES                                          
         XC    LASTMG(LASTMED-LASTMG+1),LASTMG                                  
         BAS   RE,SETGO            SETUP GOBLOCK                                
         MVC   GOSELCLI,SORTCLI                                                 
         MVC   GOSELPRO,SORTPRO                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   PPRO,SORTPRO                                                     
         MVC   PCLI,SORTCLI                                                     
         BAS   RE,PRINT                                                         
*                                                                               
PRNT080  OC    SORTMG,SORTMG       MEDIA GROUP ?                                
         BZ    PRNT100                                                          
         CLC   LASTMG,SORTMG       SAME AS BEFORE ?                             
         MVC   LASTMG,SORTMG                                                    
         BE    PRNT100             YES                                          
         XC    LASTMED,LASTMED                                                  
         BAS   RE,SETGO            SETUP GOBLOCK                                
         MVC   GOSELMG,SORTMG                                                   
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   PMG,SORTMG                                                       
         BAS   RE,PRINT                                                         
*                                                                               
PRNT100  OC    SORTMED,SORTMED     MEDIA ?                                      
         BZ    PRNT120                                                          
         CLC   LASTMED,SORTMED     SAME AS BEFORE ?                             
         MVC   LASTMED,SORTMED                                                  
         BE    PRNT120             YES                                          
         BAS   RE,SETGO            SETUP GOBLOCK                                
         MVC   GOSELMED,SORTMED                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   PMED,SORTMED                                                     
         BAS   RE,PRINT                                                         
*                                                                               
PRNT120  OC    SORTJOB,SORTJOB     JOB ?                                        
         BZ    PRNTX                                                            
         BAS   RE,SETGO            SETUP GOBLOCK FOR JOB                        
         MVC   GOSELCLI,SORTCLI                                                 
         MVC   GOSELPRO,SORTPRO                                                 
         MVC   GOSELJOB,SORTJOB                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVC   PJOB,SORTJOB                                                     
         MVC   PPRO,SORTPRO                                                     
         MVC   PCLI,SORTCLI                                                     
         BAS   RE,PRINT                                                         
*                                                                               
PRNTX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
READHI   ST    RE,SAVERE                                                        
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
READNXT  ST    RE,SAVERE                                                        
         OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
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
         L     RF,GOAFROM                                                       
         MVC   PFROM,0(RF)                                                      
         LA    R0,LEVELS                                                        
         LA    RF,LEVTAB                                                        
         CLC   PFROM(1),0(RF)                                                   
         BE    *+16                                                             
         LA    RF,L'LEVTAB(RF)                                                  
         BCT   R0,*-14                                                          
         B     *+10                                                             
         MVC   PFROM(2),1(RF)                                                   
*                                                                               
         EDIT  (P4,GOAGYCOM),(8,PRATE),4,ALIGN=RIGHT,DROP=2                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SPACES            CLEAR FOR NEXT TIME                          
         B     XIT                                                              
         EJECT                                                                  
SETGO    NTR1                                                                   
         XC    GOBLOCK(GOPTIONS-GOBLOCK),GOBLOCK                                
         MVC   GOSELCUL,CUL                                                     
         MVC   GOADM,DATAMGR                                                    
         MVC   GOAFROM,=AL4(AFROM)                                              
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                                                                   
         LA    R3,H1+48            DEAL WITH HEADING                            
         MVC   0(32,R3),=CL32'PRODUCTION RULES REPORT'                          
         GOTO1 CENTER,DMCB,(R3),32                                              
         GOTO1 UNDERLIN,DMCB,(32,(R3)),(X'BF',132(R3))                          
         MVC   H7+1(6),=C'OGROUP'                                               
         MVC   H7+10(6),=C'OFFICE'                                              
         MVC   H7+19(6),=C'CLIENT'                                              
         MVC   H7+28(7),=C'PRODUCT'                                             
         MVC   H7+38(6),=C'MGROUP'                                              
         MVC   H7+47(5),=C'MEDIA'                                               
         MVC   H7+55(3),=C'JOB'                                                 
         MVC   H7+64(4),=C'RATE'                                                
         MVC   H7+72(4),=C'FROM'                                                
*                                                                               
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXREQ,C' '                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+17,C'C'                                                  
         MVI   BOXCOLS+26,C'C'                                                  
         MVI   BOXCOLS+36,C'C'                                                  
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+53,C'C'                                                  
         MVI   BOXCOLS+62,C'C'                                                  
         MVI   BOXCOLS+70,C'C'                                                  
         MVI   BOXCOLS+77,C'R'                                                  
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,23,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(23)'                                  
*                                                                               
SORTREC  DS    0CL23                                                            
SORTOG   DS    C                                                                
SORTOFF  DS    CL2                                                              
SORTCLI  DS    CL6                                                              
SORTPRO  DS    CL6                                                              
SORTMG   DS    C                                                                
SORTMED  DS    C                                                                
SORTJOB  DS    CL6                                                              
*                                                                               
LASTSORT DS    0CL17                                                            
LASTOG   DS    C                                                                
LASTOFF  DS    CL2                                                              
LASTCLI  DS    CL6                                                              
LASTPRO  DS    CL6                                                              
LASTMG   DS    C                                                                
LASTMED  DS    C                                                                
*                                                                               
XIT      XIT1                                                                   
*                                                                               
SAVERE   DS    A                                                                
         SPACE 3                                                                
LEVTAB   DS    0CL3                                                             
         DC    C'D',C'DF'                                                       
         DC    C'A',C'AG'                                                       
         DC    C'G',C'OG'                                                       
         DC    C'O',C'OF'                                                       
         DC    C'C',C'CL'                                                       
         DC    C'P',C'PR'                                                       
         DC    C'J',C'JB'                                                       
LEVELS   EQU   (*-LEVTAB)/L'LEVTAB                                              
         SPACE 1                                                                
AFROM    DS    CL(GOFROML)                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
MYDX     EQU   *                                                                
         EJECT                                                                  
PRINTD   DSECT                                                                  
         DS    CL3                                                              
POG      DS    CL1                                                              
         DS    CL8                                                              
POFF     DS    CL2                                                              
         DS    CL5                                                              
PCLI     DS    CL6                                                              
         DS    CL3                                                              
PPRO     DS    CL6                                                              
         DS    CL6                                                              
PMG      DS    CL1                                                              
         DS    CL8                                                              
PMED     DS    CL1                                                              
         DS    CL5                                                              
PJOB     DS    CL6                                                              
         DS    CL2                                                              
PRATE    DS    CL8                                                              
         DS    CL1                                                              
PFROM    DS    CL4                                                              
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
       ++INCLUDE ACPROD1D                                                       
SAVEKEY  DS    CL42                                                             
EXLEN    DS    XL1                                                              
NEGSW    DS    CL1                                                              
*                                                                               
REQOGR   DS    XL1                                                              
REQOFF   DS    XL2                                                              
REQMED   DS    XL1                                                              
REQMGR   DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACPRO21   04/10/15'                                      
         END                                                                    

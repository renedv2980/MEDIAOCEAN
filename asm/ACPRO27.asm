*          DATA SET ACPRO27    AT LEVEL 033 AS OF 06/19/08                      
*PHASE T60B27A                                                                  
*INCLUDE GETUSR                                                                 
         TITLE 'T60B27 - PRODUCTION USER DATA UPDATE REPORT'                    
T60B27   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDX-MYD),*T60B27*,RA,RR=R2,CLEAR=YES                           
         LR    R5,RC                                                            
         USING MYD,R5                                                           
         ST    R2,RELO                                                          
         SR    R2,R2                                                            
         L     RF,=V(GETUSR)                                                    
         A     RF,RELO                                                          
         ST    RF,GETUSR                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         MVC   USRADATA,DATAMGR                                                 
         MVC   USRCUL,CUL                                                       
         MVC   USRABIN,BINSRCH                                                  
         MVI   REQUEST,C' '                                                     
         MVI   ONOFF,X'00'                                                      
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         EJECT                                                                  
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         CLI   PROUPT,C'Y'                                                      
         BNE   XIT                                                              
         MVI   REQUEST,C'C'        CLOSE THE BOX                                
         BAS   RE,HOOK                                                          
         MVC   P,SPACES            CLEAR PRINT LINE                             
         MVI   ONOFF,C'Y'          NO MORE BOXES                                
         BAS   RE,PRINT                                                         
         MVC   P(30),=C'ALL REQUIRED UPDATES COMPLETED'                         
         BAS   RE,PRINT                                                         
         B     XIT                                                              
         EJECT                                                                  
VREC     NTR1                                                                   
         MVI   OPTION,C'Y'                                                      
         LA    R2,PROUPTH                                                       
         CLI   PROUPT,C'Y'                                                      
         BNE   VREC020                                                          
         MVI   ERROR,INVPRINT                                                   
         CLI   WHEN,X'10'                                                       
         BNE   ERREND                                                           
         B     VREC040                                                          
*                                                                               
VREC020  OC    PROUPT,PROUPT       ASSUME 'N' IF NOT ENTERED                    
         BZ    VREC040                                                          
         MVI   ERROR,INVALID       PREPARE FOR ERROR IF NOT 'N'                 
         CLI   PROUPT,C'N'                                                      
         BNE   ERREND                                                           
*                                                                               
VREC040  MVC   CLICODE,SPACES                                                   
         MVC   PRODCODE,SPACES                                                  
         MVC   JOBNUM,SPACES                                                    
*                                                                               
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
*                                                                               
         LA    R2,PROOGRH          OFFICE GROUP IS OPTIONAL                     
         XC    REQOGR,REQOGR                                                    
         CLI   5(R2),0                                                          
         BE    VREC060                                                          
         GOTO1 VALOG                                                            
         MVC   REQOGR,8(R2)                                                     
*                                                                               
VREC060  LA    R2,PROOFFH          OFFICE IS OPTIONAL                           
         XC    REQOFC,REQOFC                                                    
         CLI   5(R2),0                                                          
         BE    VREC100                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 ANY                                                              
         MVI   NEGSW,C'N'                                                       
         CLI   8(R2),C'*'          TEST FOR NEGATIVE OFFICE                     
         BNE   VREC080                                                          
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BZ    INVEND                                                           
         STC   R1,5(R2)                                                         
         MVC   PROOFF,WORK+1       SHIFT DATA OVER 1 BYTE                       
*                                                                               
VREC080  GOTO1 VALOFF                                                           
         MVC   REQOFC,EFFOFFC      SAVE THE OFFICE                              
         CLI   NEGSW,C'Y'                                                       
         BNE   *+8                                                              
         NI    REQOFC,X'FF'-X'40'                                               
*                                                                               
VREC100  LA    R2,PROCLIH          CLIENT IS OPTIONAL                           
         MVC   CLICODE,=CL6'A'                                                  
         MVI   EXLEN,2                                                          
         CLI   5(R2),0                                                          
         BE    VREC120                                                          
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
VREC120  LA    R2,PROPROH          PRODUCT IS OPTIONAL                          
         MVC   PRODCODE,=CL6'A'                                                 
         CLI   5(R2),0                                                          
         BE    VREC140                                                          
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
VREC140  LA    R2,PROJOBH          JOB IS OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VREC160                                                          
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
VREC160  LA    R2,PROMGRH          MEDIA GROUP                                  
         XC    REQMGR,REQMGR                                                    
         CLI   5(R2),0                                                          
         BE    VREC180                                                          
         GOTO1 VALMG                                                            
         MVC   REQMGR,8(R2)                                                     
*                                                                               
VREC180  LA    R2,PROMEDH          MEDIA                                        
         XC    REQMED,REQMED                                                    
         CLI   5(R2),0                                                          
         BE    VREC200                                                          
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   REQMED,8(R2)                                                     
*                                                                               
VREC200  XC    REQUSR,REQUSR       USER IS OPTIONAL                             
         B     VREC220             SKIP THIS LOGIC FOR NOW                      
*                                                                               
         LA    R2,PROUSRH                                                       
         CLI   5(R2),0                                                          
         BE    VREC220                                                          
         MVC   REQUSR,8(R2)                                                     
         CLI   8(R2),C'*'                                                       
         BNE   VREC220                                                          
         MVC   REQUSR,9(R2)                                                     
         NI    REQUSR,X'FF'-X'40'                                               
*                                                                               
VREC220  MVI   REQTYP,C' '         TYPE IS OPTIONAL                             
         LA    R2,PROTYPH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'M'                                                       
         BE    VREC240                                                          
         CLI   8(R2),C'U'                                                       
         BE    VREC240                                                          
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
*                                                                               
VREC240  MVC   REQTYP,8(R2)                                                     
         B     XIT                                                              
         EJECT                                                                  
PREP     NTR1                                                                   
         MVC   USRBUFFL,=F'250000'  PASS BUFFER LENGTH                          
         L     R0,USRBUFFL                                                      
         GETMAIN R,LV=(0)                                                       
         ST    R1,USRABUFF          AND ADDRESS                                 
         LR    R2,R1                                                            
         LR    RE,R2                                                            
         L     RF,USRBUFFL                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                AND CLEAR IT                                
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
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
PREP020  BAS   RE,READHI                                                        
*                                                                               
PREP040  ZIC   R1,EXLEN            CHECK C/B ON CLIENT/PRODUCT                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PREP340                                                          
         ZIC   R1,LCLI             IS THIS A CLIENT?                            
         LA    R1,KEY+3(R1)                                                     
         CLI   0(R1),C' '                                                       
         BH    PREP060                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVEKEY,KEY                                                      
         GOTO1 SETCLI                                                           
         GOTO1 SETOG                                                            
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,REREAD                                                        
         BAS   RE,READNXT                                                       
         B     PREP040                                                          
*                                                                               
PREP060  ZIC   R1,LCLIPRO          IS THIS A PRODUCT?                           
         LA    R1,KEY+3(R1)                                                     
         CLI   0(R1),C' '                                                       
         BH    PREP080                                                          
         MVC   SAVEKEY,KEY                                                      
         GOTO1 SETPROD                                                          
         GOTO1 SETOG                                                            
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,REREAD                                                        
         BAS   RE,READNXT                                                       
         B     PREP040                                                          
*                                                                               
PREP080  BAS   RE,CHKOGR           IS OFFICE GROUP CORRECT ?                    
         BNE   PREP320             NO, SKIP IT                                  
         BAS   RE,CHKOFF           YES, IS OFFICE CORRECT ?                     
         BNE   PREP320             NO, SKIP IT                                  
*                                                                               
PREP100  GOTO1 SETJOB              YES, SET MEDIA AND MEDIA GROUP               
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
         MVC   KEY,SAVEKEY         YES, RESTORE THE KEY                         
         BAS   RE,CHKMGR           IS MEDIA GROUP CORRECT ?                     
         BNE   PREP320             NO, SKIP IT                                  
         BAS   RE,CHKMED           YES, IS MEDIA CORRECT ?                      
         BNE   PREP320             NO, SKIP IT                                  
         BAS   RE,REREAD           RE-READ THE JOB RECORD                       
         SPACE 1                                                                
         XC    USRCUT,USRCUT       CLEAR CUTOFF DATE FIELD                      
         MVI   ELCODE,X'26'        GET JOB ELEMENT                              
         BAS   RE,GETELIO                                                       
         BNE   PREP105                                                          
         USING ACJOBD,R6                                                        
         MVC   USRCUT,ACJBSTRT     GET START DATE                               
         CLI   ACJBLEN,ACJBLNQ2    IS ELEMENT TOO SMALL FOR OPEN DATE ?         
         BL    PREP105             YES                                          
         NC    ACJBOPND,ACJBOPND   NO, IS THERE AN OPEN DATE ?                  
         BZ    PREP105             NO                                           
         MVC   USRCUT,ACJBOPND     YES, USE THAT ONE AS CUTOFF                  
         SPACE 1                                                                
PREP105  CLI   PROCLO,C'Y'         INCLUDE CLOSED JOBS ?                        
         BE    PREP110             YES                                          
         MVI   ELCODE,ACSTELQ      NO, GET STATUS ELEMENT                       
         BAS   RE,GETELIO                                                       
         BNE   PREP110                                                          
         USING ACSTATD,R6                                                       
         TM    ACSTSTAT,X'40'      IS JOB CLOSED ?                              
         BO    PREP320             YES, SKIP IT                                 
         SPACE 1                                                                
PREP110  CLI   PROUPT,C'Y'         IF UPDATING, DELETE ALL A2 ELEMENTS          
         BNE   PREP120                                                          
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETELIO                                                       
         BNE   PREP120                                                          
         GOTO1 REMELEM                                                          
*                                                                               
PREP120  MVC   USRCLI,CLICODE                                                   
         MVC   USRPRO,PRODCODE                                                  
         MVC   USRJOB,JOBNUM                                                    
         MVC   USRMG,MGROUP                                                     
         MVC   USRMED,MEDIA                                                     
         MVC   USROFG,EFFOFG                                                    
         MVC   USROFC,EFFOFFC                                                   
         GOTO1 GETUSR,DMCB,USRBLOCK                                             
         OC    USRJOBCT,USRJOBCT                                                
         BZ    PREP300                                                          
         L     R2,USRJOBCT         GET NUMBER OF ENTRIES                        
         L     R4,USRAJOBT                                                      
         USING JOBRECD,R4                                                       
         LA    R4,JOBA2                                                         
         USING ACUFD,R4                                                         
         CLI   REQTYP,C' '                                                      
         BE    PREP140                                                          
         CLC   REQTYP,USRJOBST                                                  
         BNE   PREP320                                                          
PREP140  LA    R3,P                                                             
         USING PROHEAD,R3                                                       
         MVC   PROCLNT,CLICODE                                                  
         MVC   PROPROD,PRODCODE                                                 
         MVC   PROJOBNB,JOBNUM                                                  
         MVC   PROSTAR,USRJOBST                                                 
*                                                                               
PREP160  TM    ACUFSTAT,X'04'      IS THIS A SHOW ON AUTH?                      
         BO    PREP240             YES, SKIP IT                                 
*                                                                               
         OC    REQUSR,REQUSR                                                    
         BZ    PREP200                                                          
*                                                                               
         MVC   WORK(L'REQUSR),REQUSR                                            
         OI    WORK,X'40'          FORCE UPPER CASE BIT ON                      
         TM    REQUSR,X'40'        TEST FOR POSITIVE FILTER                     
         BO    PREP170             YES                                          
*                                                                               
         CLC   ACUFCODE,WORK       NEGATIVE FILTER                              
         BE    PREP180             YES-SKIP IT                                  
         B     PREP200             NO-TAKE IT                                   
*                                                                               
PREP170  CLC   ACUFCODE,WORK       POSITIVE FILTERING                           
         BE    PREP200                                                          
*                                                                               
PREP180  LA    R4,JOBLNG(R4)                                                    
         BCT   R2,PREP160                                                       
         B     PREP320                                                          
*                                                                               
PREP200  MVC   PROCODE,ACUFCODE                                                 
         MVC   PRODESC,ACUFDESC     SHOW DESCRIPTION IN TITLES                  
         MVC   PRODATA,SPACES                                                   
         CLI   ACUFLEN,32                                                       
         BE    PREP220                                                          
         CLI   ACUFDATA,X'40'      IS THERE SIGNIFICANT DATA                    
         BNH   PREP220                                                          
         ZIC   R1,ACUFLEN                                                       
         SH    R1,=H'33'                                                        
         BM    PREP220                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRODATA(0),ACUFDATA                                              
*                                                                               
PREP220  MVC   PROEDT,ACUFEDIT                                                  
         EDIT  (1,ACUFMXLN),(2,PROMAX)                                          
         MVI   PROREQ,C'N'                                                      
         TM    ACUFSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   PROREQ,C'Y'                                                      
         MVI   PRONED,C'N'                                                      
         TM    ACUFSTAT,X'40'                                                   
         BNO   *+8                                                              
         MVI   PRONED,C'Y'                                                      
         MVI   PROEST,C'N'                                                      
         TM    ACUFSTAT,X'20'                                                   
         BNO   *+8                                                              
         MVI   PROEST,C'Y'                                                      
         MVI   PROBIL,C'N'                                                      
         TM    ACUFSTAT,X'10'                                                   
         BNO   *+8                                                              
         MVI   PROBIL,C'Y'                                                      
         MVI   PROREC,C'N'                                                      
         TM    ACUFSTAT,X'08'                                                   
         BNO   *+8                                                              
         MVI   PROREC,C'Y'                                                      
         BAS   RE,PRINT                                                         
         CLI   PROUPT,C'Y'                                                      
         BNE   PREP240                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(L'JOBA2+L'JOBDATA),0(R4)                                 
*                                                                               
         MVI   ERROR,0                                                          
         MVI   ERROPT,C'Y'         COME BACK IN CASE OF ERROR                   
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   ERROR,0             EVERYTHING OK                                
         BE    PREP240             YES                                          
*                                  NO, RECORD WILL BE TOO LONG                  
         MVC   P+1(44),=C'NO UPDATE PERFORMED - JOB RECORD IS TOO LONG'         
         B     PREP260                                                          
*                                                                               
PREP240  LA    R4,JOBLNG(R4)                                                    
         MVC   PROCLNT,SPACES                                                   
         MVC   PROPROD,SPACES                                                   
         MVC   PROJOBNB,SPACES                                                  
         MVI   PROSTAR,C' '                                                     
         BCT   R2,PREP160          GET ALL ENTRIES                              
         MVC   P,SPACES                                                         
         BAS   RE,PRINT            SKIP A LINE                                  
         CLI   USRJOBST,C'M'       MORE THAN 10 USE FIELDS ?                    
         BNE   PREP300              NO, SEE IF WE NEED TO UPDATE                
         CLI   PROUPT,C'Y'                                                      
         BE    PREP280                                                          
         MVC   P+1(72),=C'THIS JOB HAS MORE THAN 10 USER FIELDS - MAKE X        
               CORRECTIONS BEFORE UPDATING'                                     
*                                                                               
PREP260  BAS   RE,PRINT                                                         
         MVC   P,SPACES                                                         
         BAS   RE,PRINT                                                         
         B     PREP320                                                          
*                                                                               
PREP280  MVC   P+1(54),=C'NO UPDATE PERFORMED - JOB HAS MORE THAN 10 USX        
               ER FIELDS'                                                       
         B     PREP260                                                          
*                                                                               
PREP300  CLI   PROUPT,C'Y'                                                      
         BNE   PREP320                                                          
         CLI   USRJOBST,C'U'       DOES THIS JOB NEED AN UPDATE ?               
         BNE   PREP320              NO, DON'T BOTHER THEN                       
         BAS   RE,UPDATE                                                        
*                                                                               
PREP320  ZIC   R1,LCLIJOB                                                       
         LA    R1,KEY+3-1(R1)                                                   
         ZIC   RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R1)                                                         
         B     PREP020             THEN GET NEXT RECORD                         
*                                                                               
PREP340  L     R1,USRABUFF                                                      
         L     R0,USRBUFFL                                                      
         FREEMAIN R,A=(1),LV=(0)                                                
         B     XIT                                                              
         EJECT                                                                  
CHKOFF   CLC   REQOFC,SPACES                                                    
         BNH   CHKOFFY                                                          
         CLC   REQOFC,EFFOFFC                                                   
         BE    CHKOFFY                                                          
         TM    REQOFC,X'40'                                                     
         BO    CHKOFFN                                                          
         MVC   HALF,REQOFC                                                      
         OI    HALF,X'40'                                                       
         CLC   HALF,EFFOFFC                                                     
         BNE   CHKOFFY                                                          
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
         SPACE 3                                                                
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
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
UPDATE   NTR1                                                                   
         L     R6,AIO                                                           
         USING ACKEYD,R6                                                        
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'ACCOUNT',(R6),(R6),DMWORK              
         CLI   DMCB+8,0                                                         
         BNE   DUMP                                                             
         B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                                                                   
         LA    R3,H1+48            DEAL WITH HEADING                            
         MVC   0(32,R3),=CL32'USER UPDATE REPORT'                               
         GOTO1 CENTER,DMCB,(R3),32                                              
         GOTO1 UNDERLIN,DMCB,(32,(R3)),(X'BF',132(R3))                          
         MVC   H7+2(6),=C'CLIENT'                                               
         MVC   H7+13(6),=C'PRODCT'                                              
         MVC   H7+24(3),=C'JOB'                                                 
         MVC   H7+34(1),=C'S'                                                   
         MVC   H7+38(2),=C'CD'                                                  
         MVC   H7+44(12),=C'FIELD HEADER'                                       
         MVC   H7+61(4),=C'DATA'                                                
         MVC   H7+94(3),=C'EDT'                                                 
         MVC   H7+98(3),=C'MAX'                                                 
         MVC   H8+98(3),=C'LEN'                                                 
         MVC   H7+102(3),=C'REQ'                                                
         MVC   H8+102(3),=C'FLD'                                                
         MVC   H7+106(3),=C'REQ'                                                
         MVC   H8+106(3),=C'BIL'                                                
         MVC   H7+110(3),=C'SHW'                                                
         MVC   H8+110(3),=C'EST'                                                
         MVC   H7+114(3),=C'SHW'                                                
         MVC   H8+114(3),=C'BIL'                                                
         MVC   H7+118(3),=C'SHW'                                                
         MVC   H8+118(3),=C'REC'                                                
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVC   BOXREQ,REQUEST                                                   
         MVC   BOXOFF,ONOFF                                                     
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+10,C'C'                                                  
         MVI   BOXCOLS+21,C'C'                                                  
         MVI   BOXCOLS+32,C'C'                                                  
         MVI   BOXCOLS+36,C'C'                                                  
         MVI   BOXCOLS+41,C'C'                                                  
         MVI   BOXCOLS+58,C'C'                                                  
         MVI   BOXCOLS+93,C'C'                                                  
         MVI   BOXCOLS+97,C'C'                                                  
         MVI   BOXCOLS+101,C'C'                                                 
         MVI   BOXCOLS+105,C'C'                                                 
         MVI   BOXCOLS+109,C'C'                                                 
         MVI   BOXCOLS+113,C'C'                                                 
         MVI   BOXCOLS+117,C'C'                                                 
         MVI   BOXCOLS+121,C'R'                                                 
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
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
         SPACE 3                                                                
INVEND   MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 3                                                                
DUMP     DC    H'0'                                                             
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
RELO     DS    A                                                                
GETUSR   DS    A                                                                
SAVERE   DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
       ++INCLUDE ACUSRBLOCK                                                     
MYDX     EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACGETUSRD                                                      
         EJECT                                                                  
PROHEAD  DSECT                                                                  
         DS    CL2                                                              
PROCLNT  DS    CL6                                                              
         DS    CL5                                                              
PROPROD  DS    CL6                                                              
         DS    CL5                                                              
PROJOBNB DS    CL6                                                              
         DS    CL4                                                              
PROSTAR  DS    CL1                                                              
         DS    CL3                                                              
PROCODE  DS    CL2                                                              
         DS    CL4                                                              
PRODESC  DS    CL12                                                             
         DS    CL5                                                              
PRODATA  DS    CL30                                                             
         DS    CL4                                                              
PROEDT   DS    CL1                                                              
         DS    CL2                                                              
PROMAX   DS    CL2                                                              
         DS    CL3                                                              
PROREQ   DS    CL1                                                              
         DS    CL3                                                              
PRONED   DS    CL1                                                              
         DS    CL3                                                              
PROEST   DS    CL1                                                              
         DS    CL3                                                              
PROBIL   DS    CL1                                                              
         DS    CL3                                                              
PROREC   DS    CL1                                                              
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
       ++INCLUDE ACPROD7D                                                       
SAVEKEY  DS    CL42                                                             
EXLEN    DS    XL1                                                              
REQUEST  DS    CL1                                                              
ONOFF    DS    XL1                                                              
NEGSW    DS    CL1                                                              
*                                                                               
REQOGR   DS    XL1                                                              
REQOFC   DS    XL2                                                              
REQMED   DS    XL1                                                              
REQMGR   DS    XL1                                                              
REQTYP   DS    XL1                                                              
REQUSR   DS    CL2                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACPRO27   06/19/08'                                      
         END                                                                    

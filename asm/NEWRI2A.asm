*          DATA SET NEWRI2A    AT LEVEL 096 AS OF 05/01/02                      
*PHASE T3202AA                                                                  
         TITLE 'T3202A - PRE-BUY 1 (PB1) FOR BBDO'                              
         PRINT NOGEN                                                            
T3202A   CSECT                                                                  
         NMOD1 0,**PB1***                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T3202A+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R6,ASYSD                                                         
         USING NETSYSD,R6                                                       
         L     R7,ANETWS2                                                       
         USING PRED,R7                                                          
         SPACE 1                                                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         SPACE 1                                                                
         LA    R1,HOOK                                                          
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         LA    R1,PBSPECS                                                       
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         SPACE 1                                                                
         L     R1,=A(NETLIST)      PROVIDE NETWORK LIST                         
         A     R1,RELO                                                          
         ST    R1,NBANBUFF                                                      
         SPACE 1                                                                
         L     R1,=A(PROGBUFF)     ADDRESS PROGRAM BUFF                         
         A     R1,RELO                                                          
         ST    R1,APBUFF                                                        
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 3                                                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         MVI   ANYSORT,C'N'                                                     
         SPACE 1                                                                
         MVI   NBDATA,C'B'         READ UNIT AND PACKAGE RECORDS                
         OI    NBSPLOPT,X'80'      TURN ON SPLIT OPTION                         
         MVI   NBHUNOPT,C'Y'       WE CAN DEAL IN HUNDREDS                      
         MVI   NBRESUME,NBPROCPK   RESUME READING PACKAGES                      
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBSELUOP,C'E'                                                    
         CLI   ACTOPT,C'N'         ACTUAL SCHEDULE OPTION                       
         BE    INIT1                                                            
         MVI   NBSELUOP,C'A'                                                    
         MVI   NBESTOPT,C'A'                                                    
         CLI   ACTOPT,C'2'                                                      
         BE    INIT1                                                            
         MVI   NBACTOPT,C'Y'                                                    
         SPACE 1                                                                
INIT1    CLI   PFBOPT,C'Y'         PFB DEMO OPTION                              
         BNE   *+8                                                              
         MVI   NBESTOPT,C'P'                                                    
         MVI   NUMDEMS,0           SET UP NUMBER OF DEMOS                       
         CLI   NDDEMOS+2,0                                                      
         BE    INIT2                                                            
         MVI   NUMDEMS,1                                                        
         CLI   NDDEMOS+5,0                                                      
         BE    INIT2                                                            
         MVI   NUMDEMS,2                                                        
         CLI   NDDEMOS+8,0                                                      
         BE    INIT2                                                            
         MVI   NUMDEMS,3                                                        
         SPACE 1                                                                
INIT2    XC    DETDISP,DETDISP     FIGURE DISPACEMENT FOR MAIN REPORTS          
         CLI   LEFTOPT,C'Y'                                                     
         BE    LOOP                                                             
         CLI   NUMDEMS,2                                                        
         BH    LOOP                                                             
         MVI   DETDISP+3,9                                                      
         BE    LOOP                                                             
         MVI   DETDISP+3,18                                                     
         EJECT                                                                  
*              MAIN NETIO LOOP                                                  
         SPACE 3                                                                
LOOP     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCPK                                                  
         BE    GOTPACK                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTUN                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    LASTONE                                                          
         B     LOOP                                                             
         SPACE 1                                                                
GOTDATE  XC    PERTYPE,PERTYPE     SET UP WEEK LIST                             
         MVI   PERTYPE,C'W'                                                     
         XC    WLIST,WLIST                                                      
         XC    WLISTSP,WLISTSP                                                  
         NETGO NVWKLST,DMCB,=F'64',WLIST,PERTYPE                                
         B     LOOP                                                             
         SPACE 1                                                                
GOTUN    BAS   RE,UNIT                                                          
         B     LOOP                                                             
         SPACE 1                                                                
GOTPACK  BAS   RE,PACKAGE                                                       
         B     LOOP                                                             
         SPACE 1                                                                
LASTONE  BAS   RE,REPORTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              PROCESS PACKAGE RECORDS                                          
         SPACE 3                                                                
PACKAGE  NTR1                                                                   
         XC    SORTAREA,SORTAREA   FILL SORT RECORD FOR PACKAGE                 
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         MVC   SEST,NPKEST         TAKE EST/NET/PACK FROM KEY                   
         MVC   SNET,NPKNET                                                      
         MVC   SPACK,NPKPACK                                                    
         DROP  R4                                                               
         SPACE 1                                                                
         MVI   STYPE,1             (PACKAGE RECORDS ARE TYPE 1)                 
         MVC   SPNAME,NBPAKNAM     AND THE REST FROM NETBLOCK                   
         MVC   SPCOST,NBPAKCST                                                  
         MVC   SPGCPM,NBPAKCPM                                                  
         MVC   SPDP,NBDPNAM                                                     
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         SPACE 3                                                                
*                                  PUT AND TRACE SORT RECORDS                   
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         MVI   ANYSORT,C'Y'                                                     
         B     XIT                 PATCH OUT FOR TRACE                          
         MVC   P,SORTAREA                                                       
         GOTO1 HEXOUT,DMCB,P,P2,132,=C'SEP'                                     
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS UNIT RECORDS                                             
         SPACE 3                                                                
UNIT     NTR1                                                                   
         CLI   ACTOPT,C'3'                                                      
         BE    UNITA                                                            
         OC    NBESTUN,NBESTUN     UNIT MUST BE ACTIVE                          
         BZ    XIT                                                              
         B     UNITB                                                            
         SPACE 1                                                                
UNITA    OC    NBACTUN,NBACTUN                                                  
         BZ    XIT                                                              
         SPACE 1                                                                
UNITB    XC    SORTAREA,SORTAREA   FILL SORT AREA WITH UNIT DETAILS             
         MVC   SEST,NBACTEST                                                    
         MVC   SNET,NBACTNET                                                    
         MVC   SPACK,NBPACK                                                     
         MVI   STYPE,2                                                          
         MVC   SPROG,NBPROGNM                                                   
         OC    SPROG,SPACES                                                     
         MVC   SLEN,NBLEN                                                       
         MVC   SSURVEY,NBSURVEY                                                 
         SPACE 1                                                                
         LA    R2,WLIST            LOOK UP WEEK NUMBER                          
         LA    R3,1                                                             
         SPACE 1                                                                
UNIT2    STC   R3,SWEEK                                                         
         CLC   NBACTDAT,2(R2)      (CHECK V END DATE)                           
         BNH   UNIT4                                                            
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R2),0                                                          
         BNE   UNIT2                                                            
         SPACE 1                                                                
UNIT4    LA    R2,SLLIST           NOW ANALYZE SPOT LENGTH                      
         LA    R3,1                                                             
         SPACE 1                                                                
UNIT6    STC   R3,SLENNO                                                        
         CLC   SLEN,0(R2)                                                       
         BE    UNIT8                                                            
         CLI   0(R2),X'FF'                                                      
         BE    UNIT8                                                            
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         B     UNIT6                                                            
         SPACE 1                                                                
UNIT8    LA    R5,SACCUMS                                                       
         USING ACCUMD,R5                                                        
         MVC   ACUN+2(2),NBESTUN   UNITS                                        
         MVC   ACHUT+2(2),NBESTHUT HUT                                          
         MVC   ACSHR+2(2),NBESTSHR SHARE                                        
         CLI   ACTOPT,C'3'                                                      
         BNE   UNIT9                                                            
         MVC   ACUN+2(2),NBACTUN   UNITS                                        
         MVC   ACHUT+2(2),NBACTHUT HUT                                          
         MVC   ACSHR+2(2),NBACTSHR SHARE                                        
         SPACE 1                                                                
UNIT9    CLI   NBN0B2,0            IF USER IS SET UP TO EQUIVALENCE             
         BE    UNIT10                                                           
         BAS   RE,DEMPOST          POST EQUIVALENT DATA                         
         MVC   SAVIMPE,NBN0B2      SAVE PROFILE BYTES FOR EQUIVALENCY           
         MVC   SAVGRPE,NBN2B1                                                   
         MVI   NBN0B2,0            SET UP TO GET RAW DATA                       
         MVI   NBN2B1,0                                                         
         GOTO1 NBNETVAL,DMCB,NETBLOCK        (REVALUE)                          
         LA    R5,ACRAW-ACHOMVPH(R5)                                            
         BAS   RE,DEMPOST          AND POST RAW DATA                            
         BAS   RE,PUTSORT          THEN WE'RE DONE                              
         MVC   NBN0B2,SAVIMPE      RESTORE EQUIVALENCY                          
         MVC   NBN2B1,SAVGRPE                                                   
         B     XIT                                                              
         SPACE 1                                                                
UNIT10   LA    R5,ACRAW-ACHOMVPH(R5)    USER IS NOT SET UP TO EQUIV             
         BAS   RE,DEMPOST          POST RAW DATA                                
         MVI   NBN0B2,30           SET UP TO GET EQUIVALENCED DATA              
         MVI   NBN2B1,30                                                        
         GOTO1 NBNETVAL,DMCB,NETBLOCK        (REVALUE)                          
         LA    R5,SACCUMS                                                       
         BAS   RE,DEMPOST          AND POST EQUIVALENCED DATA                   
         BAS   RE,PUTSORT          THEN WE'RE DONE                              
         MVI   NBN0B2,0                                                         
         MVI   NBN2B1,0                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DEMOGRAPHIC POSTING                                              
         SPACE 3                                                                
DEMPOST  NTR1                                                                   
         MVC   ACHOMIMP,NBESTHOM+4             HOME IMPRESSIONS                 
         MVC   ACHOMRTG+2(2),NBESTHOM+2        HOME RATINGS                     
         LA    R2,NDESTDEM                     SET UP FOR DEMOS                 
         ZIC   R3,NUMDEMS                                                       
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R4,ACDEMVPH                                                      
         CLI   ACTOPT,C'3'                                                      
         BNE   DEMPOST2                                                         
         MVC   ACHOMIMP,NBACTHOM+4                                              
         MVC   ACHOMRTG+2(2),NBACTHOM+2                                         
         LA    R2,NDACTDEM                                                      
         SPACE 1                                                                
DEMPOST2 MVC   2(2,R4),0(R2)                   VPH                              
         MVC   4(4,R4),4(R2)                   IMPRESSIONS                      
         MVC   10(2,R4),2(R2)                  RATINGS                          
         SPACE 1                                                                
         LA    R2,8(R2)                                                         
         LA    R4,12(R4)                                                        
         BCT   R3,DEMPOST2                                                      
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
REPORTS  NTR1                                                                   
         CLI   ANYSORT,C'Y'                                                     
         BNE   XIT                                                              
         XC    LASTREC,LASTREC                                                  
         B     REPNEXT2                                                         
         SPACE 1                                                                
REPNEXT  MVC   LASTREC,SORTAREA                                                 
         MVC   EFFEST,SEST                                                      
         MVC   EFFNET,SNET                                                      
         SPACE 1                                                                
REPNEXT2 XC    SORTAREA,SORTAREA                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    REPEOF                                                           
         MVC   SORTAREA,0(R2)                                                   
         CLI   LASTREC,0                                                        
         BE    REPDET                                                           
         CLC   SORTAREA(1),LASTREC CHECK FOR C/B                                
         BNE   ESTCB                                                            
         CLC   SORTAREA(5),LASTREC                                              
         BNE   NETCB                                                            
         CLC   SORTAREA(6),LASTREC                                              
         BNE   PACKCB                                                           
         CLC   SORTAREA(25),LASTREC                                             
         BNE   PROGCB                                                           
         B     REPDET                                                           
         SPACE 1                                                                
ESTCB    BAS   RE,PROGTOT          ESTIMATE CONTROL BREAK                       
         BAS   RE,PACKTOT                                                       
         BAS   RE,RECAP                                                         
         BAS   RE,NETTOT                                                        
         BAS   RE,ALLTOT                                                        
         B     REPDET                                                           
         SPACE 1                                                                
NETCB    BAS   RE,PROGTOT          NETWORK CONTROL BREAK                        
         BAS   RE,PACKTOT                                                       
         BAS   RE,RECAP                                                         
         BAS   RE,NETTOT                                                        
         B     REPDET                                                           
         SPACE 1                                                                
PACKCB   BAS   RE,PROGTOT          PACKAGE CONTROL BREAK                        
         BAS   RE,PACKTOT                                                       
         BAS   RE,RECAP                                                         
         B     REPDET                                                           
         SPACE 1                                                                
PROGCB   BAS   RE,PROGTOT          PROGRAM CONTROL BREAK                        
         B     REPDET                                                           
         SPACE 1                                                                
REPDET   CLI   STYPE,2             CHECK RECORD TYPE                            
         BE    REPUNIT                                                          
         BAS   RE,POSTPACK                                                      
         B     REPNEXT                                                          
         SPACE 1                                                                
REPUNIT  BAS   RE,POSTUNIT                                                      
         B     REPNEXT                                                          
         SPACE 1                                                                
REPEOF   BAS   RE,PROGTOT          END OF SORT FILE                             
         BAS   RE,PACKTOT                                                       
         BAS   RE,RECAP                                                         
         BAS   RE,NETTOT                                                        
         BAS   RE,ALLTOT                                                        
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST PACKAGE DETAILS                                  
         SPACE 3                                                                
POSTPACK NTR1                                                                   
         MVI   FIRSTPAK,C'Y'       SET FIRST TIME SWITCH                        
         MVC   SAVEPACK,SORTAREA   SAVE THE WHOLE RECORD                        
         MVC   SAVPCOST,SPCOST     SAVE PACKAGE COST TO POST WHEN               
*                                  WE GET AN ACTIVE UNIT RECORD                 
         B     XIT                                                              
         SPACE 3                                                                
ADDEM    NTR1                      ROUTINE TO ADD FROM SACCUMS TO               
*                                  ALL LEVEL TOTALS                             
         LA    R2,LTOTS                                                         
         BAS   RE,ADDEM3                                                        
         MVC   SAVELEN,SLENNO                                                   
         LA    R2,SACCUMS                                                       
         USING ACCUMD,R2                                                        
         MVC   ACDOL,SAVPCOST      ADD COST TO HIGHER LEVELS                    
         XC    SAVPCOST,SAVPCOST   BUT ONLY FOR FIRST TIME                      
         DROP  R2                                                               
         L     R2,=A(PTOTS)                                                     
         BAS   RE,ADDEM2                                                        
         L     R2,=A(NTOTS)                                                     
         BAS   RE,ADDEM2                                                        
         L     R2,=A(ATOTS)                                                     
         BAS   RE,ADDEM2                                                        
         B     XIT                                                              
         SPACE 1                                                                
ADDEM2   NTR1                                                                   
         A     R2,RELO                                                          
         BAS   RE,ADDEM3           ADD IN FOR TOTALS                            
         ZIC   R1,SAVELEN                                                       
         MH    R1,=H'120'                                                       
         AR    R2,R1                                                            
         BAS   RE,ADDEM3           THEN ONCE FOR SPOT LENGTH                    
         B     XIT                                                              
         SPACE 1                                                                
ADDEM3   NTR1                      ROUTINE ADDS SACCUM LINE TO                  
*                                  LINE ADDRESSED BY R2                         
         SPACE 1                                                                
         LA    R3,SACCUMS                                                       
         LA    R0,30                                                            
         SPACE 1                                                                
ADDEM4   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADDEM4                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST UNIT DETAILS                                     
         SPACE 3                                                                
POSTUNIT NTR1                                                                   
         CLI   FIRSTPAK,C'Y'                                                    
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   FIRSTPAK,C'N'                                                    
         BAS   RE,ADDEM            ADD DETAILS TO TOTAL AREAS                   
         MVC   SURVEY,SSURVEY      SAVE SURVEY                                  
         L     R5,APBUFF           PICK UP ADDRESS OF PAKAGE BUFFER             
         USING PWD,R5                                                           
         MVI   PWTYPE,1                                                         
         BAS   RE,POSTWEEK         POST A UNIT INTO TOTAL LINE                  
         LA    R5,L'PWENTRY(R5)                                                 
         LA    R0,200              NOW POST INTO TABLE FOR PROG/LEN             
         SPACE 1                                                                
POSTUN2  CLI   PWPROG,0            MAY BE A NEW ENTRY                           
         BE    POSTUN4                                                          
         CLC   PWPROG(17),SPROG    OR A MATCH ON PROGRAM/LENGTH                 
         BE    POSTUN4                                                          
         LA    R5,L'PWENTRY(R5)                                                 
         BCT   R0,POSTUN2                                                       
         B     XIT                                                              
         SPACE 1                                                                
POSTUN4  MVC   PWPROG(17),SPROG                                                 
         MVI   PWTYPE,2                                                         
         BAS   RE,POSTWEEK         POST A UNIT INTO DETAIL LINE                 
         CLI   HIGHWEEK,0          IS THIS THE FIRST UNIT FOR PACKAGE           
         BNE   POSTUN6                                                          
         MVC   HIGHWEEK,SWEEK      THEN IT IS BOTH HIGHEST                      
         MVC   LOWWEEK,SWEEK       AND LOWEST WEEK NUMBER                       
         B     XIT                                                              
         SPACE 1                                                                
POSTUN6  CLC   SWEEK,HIGHWEEK      IS THIS UNIT HIGHEST WEEK TO DATE            
         BNH   *+10                                                             
         MVC   HIGHWEEK,SWEEK      THEN WE NEED TO KNOW ABOUT IT                
         SPACE 1                                                                
         CLC   SWEEK,LOWWEEK       IS THIS UNIT LOWWEST WEEK TO DATE            
         BNL   XIT                                                              
         MVC   LOWWEEK,SWEEK       THEN SAVE THAT TOO                           
         B     XIT                                                              
         SPACE 1                                                                
POSTWEEK NTR1                                                                   
         LA    R1,1                ADD 1 TO TOTAL UNITS                         
         AH    R1,PWTOTAL                                                       
         STH   R1,PWTOTAL                                                       
         ZIC   R2,SWEEK            PICK UP UNIT'S WEEK NUMBER                   
         BCTR  R2,0                                                             
         SLL   R2,1                                                             
         LA    R2,PWWEEKS(R2)                                                   
         LA    R1,1                ADD 1 TO WEEKS UNITS                         
         AH    R1,0(R2)                                                         
         STH   R1,0(R2)                                                         
         B     XIT                                                              
         DROP  R5                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PROGRAM/LENGTH TOTALS                         
         SPACE 3                                                                
PROGTOT  NTR1                      PROGRAM/LENGTH TOTALS                        
         LA    R5,LASTREC          PICK UP NAME AND LENGTH FROM LAST            
         USING SORTAREA,R5         SORT RECORD                                  
         CLI   STYPE,1             IF LAST RECORD WAS A PACKAGE                 
         BE    XIT                 THEN THERE WERE NO UNITS AT ALL              
         LA    R3,P                                                             
         A     R3,DETDISP                                                       
         MVC   1(16,R3),SPROG                                                   
         EDIT  (1,SLEN),(3,18(R3))                                              
         DROP  R5                                                               
         MVI   REPTYPE,C'D'                                                     
         LA    R2,LTOTS                                                         
         MVI   TOTLEVEL,1                                                       
         USING ACCUMD,R2                                                        
         MVI   AVESW,C'N'                                                       
         CLI   AVEOPT,C'Y'         DISPLAY 'AVERAGE'                            
         BNE   PGT2                                                             
         CLC   ACUN,=F'2'          FOR 2 OR MORE UNITS                          
         BL    PGT2                                                             
         MVC   10+132(7,R3),=C'AVERAGE'                                         
         MVI   AVESW,C'Y'                                                       
         SPACE 1                                                                
PGT2     L     R1,ACHUT            AVERAGE HUT                                  
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,23(R3))                                                  
         MVI   25(R3),C' '                                                      
         L     R1,ACSHR            SHARE                                        
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,27(R3))                                                  
         MVI   29(R3),C' '                                                      
         L     R0,ACHOMRTG         AVE RTG                                      
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,30(R3)),1                                                
         SPACE 1                                                                
         MVI   EQUSW,C'N'                                                       
         MVI   CPMSW,C'N'                                                       
         BAS   RE,FORMAT                                                        
         MVC   SPACING,SPACOPT                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL OTHER TOTALS                                  
         SPACE 3                                                                
PACKTOT  NTR1                      PACKAGE TOTALS                               
         L     R2,=A(PTOTS)                                                     
         MVI   TOTLEVEL,2                                                       
         USING ACCUMD,R2                                                        
         OC    ACENTRY,ACENTRY                                                  
         BZ    XIT                                                              
         L     R5,ABOX             CLOSE BOX FOR DETAIL REPORT                  
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         DROP  R5                                                               
         MVI   0(R1),C'B'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P1AREA(16),=CL16'PACKAGE TOTALS'                                 
         MVC   P3AREA(16),SPACES                                                
         LA    R5,SAVEPACK                                                      
         USING SORTAREA,R5                                                      
         OC    SPGCPM,SPGCPM       CHECK FOR GUARANTEED CPM                     
         BZ    TOT2                                                             
         MVC   P3AREA(9),=C'GUAR.CPM='                                          
         LA    R3,P3AREA+9                                                      
         EDIT  (4,SPGCPM),(7,(R3)),2,FLOAT=$,ALIGN=LEFT                         
         DROP  R5                                                               
         B     TOT2                                                             
         SPACE 1                                                                
NETTOT   NTR1                      NETWORK TOTALS                               
         L     R2,=A(NTOTS)                                                     
         MVI   TOTLEVEL,3                                                       
         OC    ACENTRY,ACENTRY                                                  
         BZ    XIT                                                              
         MVC   P1AREA(16),=CL16'NETWORK TOTALS'                                 
         MVC   P3AREA(16),SPACES                                                
         B     TOT2                                                             
         SPACE 1                                                                
ALLTOT   NTR1                      ALL NETWORKS                                 
         L     R2,=A(ATOTS)                                                     
         MVI   TOTLEVEL,4                                                       
         OC    ACENTRY,ACENTRY                                                  
         BZ    XIT                                                              
         MVC   P1AREA(16),=CL16'ALL NETWORKS'                                   
         MVC   P3AREA(16),SPACES                                                
         B     TOT2                                                             
         EJECT                                                                  
*                                  TOTALLING - CONTROL LINES AND BOXES          
         SPACE 3                                                                
TOT2     MVI   REPTYPE,C'T'                                                     
         A     R2,RELO                                                          
         ST    R2,SAVER2                                                        
         LA    RE,120(R2)                                                       
         SR    RF,RF                                                            
         LA    R0,10                                                            
         SPACE 1                                                                
TOT3     OC    0(120,RE),0(RE)     COUNT UP ACTIVE LINES                        
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         LA    RE,120(RE)                                                       
         BCT   R0,TOT3                                                          
         STC   RF,NLINES                                                        
         ZIC   R1,LINE                                                          
         AR    R1,RF                                                            
         CH    R1,=H'52'           CAN WE SQUEEZE IN 6 MORE LINES               
         BNH   TOT4                                                             
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)     NO SO SKIP TO NEW PAGE                       
         SPACE 1                                                                
TOT4     BAS   RE,DETTITL          SET UP BOXES FOR TOTALS                      
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVC   BOXCOLS(110),DETBOX                                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXFONT,1           USE SECOND FONT FOR TOTALS                   
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         ZIC   RF,NLINES                                                        
         AR    R1,RF                                                            
         MVI   5(R1),C'B'                                                       
         DROP  R5                                                               
         GOTO1 SPOOL,DMCB,(R8)     AND PRINT IN MIDDLE OF PAGE                  
         EJECT                                                                  
*              TOTALLING - SPOT LENGTH ANALYSIS - THEN MAIN TOTALS              
         SPACE 3                                                                
         LA    R3,P+1                                                           
         A     R3,DETDISP                                                       
         L     R2,SAVER2                                                        
         LA    R2,120(R2)          SET UP FOR SPOT LENGTH ANALYSIS              
         LA    R4,SLLIST                                                        
         LA    R5,10                                                            
         SPACE 1                                                                
TOT5     OC    0(120,R2),0(R2)     SKIP IF INACTIVE                             
         BZ    TOT6                                                             
         EDIT  (1,0(R4)),(3,17(R3))                                             
         MVC   21(9,R3),=C'SEC UNITS'                                           
         CLI   0(R4),X'FF'                                                      
         BNE   *+10                                                             
         MVC   17(13,R3),=C'OTHER UNITS    '                                    
         MVI   AVESW,C'N'                                                       
         MVI   CPMSW,C'N'                                                       
         MVI   EQUSW,C'N'                                                       
         BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
TOT6     LA    R2,120(R2)                                                       
         LA    R4,1(R4)                                                         
         BCT   R5,TOT5                                                          
         SPACE 1                                                                
         L     R2,SAVER2                                                        
         LA    R4,17(R3)                                                        
         MVC   0(16,R3),P1AREA                                                  
         MVC   0(9,R4),=C'ALL UNITS'                                            
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         OC    ACDOL,ACDOL                                                      
         BZ    TOT8                                                             
         MVC   0(5,R3),=C'COST='                                                
         EDIT  (4,ACDOL),(12,5(R3)),FLOAT=$,ALIGN=LEFT                          
         SPACE 1                                                                
TOT8     MVC   0(11,R4),=C'    CPM/CPP'                                         
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         MVC   0(16,R3),P3AREA                                                  
         MVC   0(16,R4),=C'ALL EQUIVALENCED'                                    
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         MVC   0(11,R4),=C'    CPM/CPP'                                         
         MVI   AVESW,C'N'                                                       
         MVI   CPMSW,C'Y'                                                       
         MVI   EQUSW,C'Y'                                                       
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVI   BOXFONT,0           RETURN TO BASE FONT                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL FORMATTING OF DEMO TOTALS                     
         SPACE 3                                                                
*              INPUTS              R2=A(ACCUMS)                                 
*                                  AVESW=Y FOR AVERAGES                         
*                                  RAWSW=Y FOR EQUIV. AND RAW                   
*                                  CPMSW=Y FOR CPM/CPP                          
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         LA    R3,P+34                                                          
         A     R3,DETDISP                                                       
         EDIT  (4,ACUN),(4,0(R3))  NUMBER OF UNITS                              
         MVC   COST,ACDOL          SAVE TOTAL COST AT THIS LEVEL                
         MVC   UNITS,ACUN          AND UNITS                                    
         LA    R3,4(R3)                                                         
         LR    R0,R2                                                            
         LA    R2,ACRAW                                                         
         BAS   RE,FORM2            FORMAT RAW FIGURES                           
         LR    R2,R0                                                            
         CLI   EQUSW,C'Y'                                                       
         BNE   FORMEND                                                          
         LA    R3,264(R3)                                                       
         LA    R2,ACHOMVPH                                                      
         BAS   RE,FORM2            AND, OPTIONALLY EQUIVALENCY                  
         LR    R2,R0                                                            
         SPACE 1                                                                
FORMEND  XC    ACENTRY,ACENTRY                                                  
         B     XIT                                                              
         SPACE 1                                                                
FORM2    NTR1                                                                   
         ZIC   R4,NUMDEMS          (NUMBER OF DEMOS +1 FOR HOMES)               
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FORM4    BAS   RE,VPH                                                           
         LA    R2,4(R2)                                                         
         LA    R3,3(R3)                                                         
         BAS   RE,IMPS             SHOW IMPS AVE/IMPS AND CPM                   
         LA    R2,4(R2)                                                         
         LA    R3,7(R3)                                                         
         BAS   RE,GRPS             SHOW GRPS AVE/GRPS AND CPP                   
         LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,FORM4                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT VPH                                            
         SPACE 3                                                                
*              INPUT               R2=A(TOTAL VPH)                              
*                                  R3=A(DISPLAY AREA)                           
*                                  UNITS=SAVED UNITS                            
         SPACE 1                                                                
VPH      NTR1                                                                   
         CLI   REPTYPE,C'D'        SHOW VPH ON DETAIL LINES                     
         BNE   XIT                                                              
         L     R0,0(R2)                                                         
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         SRDA  R0,31                                                            
         D     R0,UNITS                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,0(R3))                                                   
         MVI   3(R3),C' '                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT IMPS, AVERAGE IMPS AND CPM                     
         SPACE 3                                                                
*              INPUT               R2=A(TOTAL IMPS)                             
*                                  R3=A(DISPLAY AREA)                           
*                                  UNITS=SAVED UNITS                            
*                                  COST=SAVED COST                              
         SPACE 1                                                                
IMPS     NTR1                                                                   
         OC    0(4,R2),0(R2)                                                    
         BZ    XIT                                                              
*                                  EDIT TOTAL IMPRESSIONS                       
         NETGO NVPRDEM,DMCB,(C'I',0),(R2),(R3)                                  
         LA    R3,132(R3)                                                       
         SPACE 1                                                                
         CLI   AVESW,C'Y'          OPTIONAL AVERAGE                             
         BNE   IMPS4                                                            
         L     R0,0(R2)                                                         
         SRDA  R0,31                                                            
         D     R0,UNITS            IMPRESSIONS/UNIT                             
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CLI   SURVEY,C'N'         IF MAJOR NETWORK                             
         BNE   IMPS2                                                            
         LA    R1,50(R1)           ROUND TO NEAREST 10000                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         M     R0,=F'100'                                                       
         SPACE 1                                                                
IMPS2    ST    R1,WORK                                                          
         NETGO NVPRDEM,DMCB,(C'I',0),WORK,(R3)                                  
         LA    R3,132(R3)                                                       
         SPACE 1                                                                
IMPS4    CLI   CPMSW,C'Y'                                                       
         BNE   XIT                                                              
         OC    COST,COST                                                        
         BZ    XIT                                                              
         L     R1,COST                                                          
         M     R0,=F'2000'                                                      
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,0(R3)),2,FLOAT=$                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT GRPS, AVERAGE GRPS AND CPP                     
         SPACE 3                                                                
*              INPUT               R2=A(TOTAL GRP)                              
*                                  R3=A(DISPLAY AREA)                           
*                                  UNITS=SAVED UNITS                            
*                                  COST=SAVED COST                              
         SPACE 1                                                                
GRPS     NTR1                                                                   
         L     R1,0(R2)            EDIT TOTAL GRPS                              
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         BAS   RE,GRPED                                                         
         LA    R3,132(R3)                                                       
         SPACE 1                                                                
         CLI   AVESW,C'Y'          OPTIONAL AVERAGE                             
         BNE   GRPS2                                                            
         M     R0,=F'2'            COMPUTE AVERAGE GRPS                         
         D     R0,UNITS                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BAS   RE,GRPED            AND EDIT THESE                               
         LA    R3,132(R3)                                                       
         SPACE 1                                                                
GRPS2    CLI   CPMSW,C'Y'          OPTIONAL CPP                                 
         BNE   XIT                                                              
         L     R1,COST                                                          
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,(R3)),FLOAT=$                                            
         CLI   0(R3),C'$'                                                       
         BNE   XIT                                                              
         MVI   0(R3),C' '                                                       
         B     XIT                                                              
         SPACE 1                                                                
GRPED    NTR1                                                                   
         EDIT  (R1),(6,0(R3)),1                                                 
         CLI   0(R3),C' '                                                       
         BE    XIT                                                              
         EDIT  (R1),(7,0(R3))                                                   
         MVI   6(R3),C' '                                                       
         B     XIT                                                              
         EJECT                                                                  
*              RECAP OF UNITS BY WEEK FOR EACH PROGRAM/LENGTH                   
         SPACE 3                                                                
RECAP    NTR1                                                                   
         CLI   HIGHWEEK,0          IGNORE IF NO ACTION                          
         BE    XIT                                                              
         MVI   REPTYPE,C'R'        NOTE THAT WE ARE DOING RECAP                 
         SPACE 1                                                                
RECAPB   ZIC   R1,HIGHWEEK         COMPUTE N'WEEKS IN RECAP                     
         ZIC   R0,LOWWEEK                                                       
         LA    R1,1(R1)                                                         
         SR    R1,R0                                                            
         STC   R1,NUMWEEKS                                                      
         MVI   ANYMORE,C'N'                                                     
         CLI   NUMWEEKS,21         MAX WE CAN DISPLAY IS 20                     
         BL    RECAP1                                                           
         MVI   NUMWEEKS,20                                                      
         MVI   ANYMORE,C'Y'                                                     
         CH    R1,=H'40'           MORE THAN 40 LEFT                            
         BH    RECAP1              SO DO 20 THIS TIME                           
         SRL   R1,1                LESS THAN 40 LEFT - DO HALF                  
         STC   R1,NUMWEEKS                                                      
         SPACE 1                                                                
RECAP1   ZIC   R1,NUMWEEKS         REPORT TAKES N'WEEKS*4                       
         SLL   R1,2                                                             
         LA    R1,28(R1)           PLUS 28 CHARACTERS TO PRINT                  
         CLI   ANYMORE,C'Y'                                                     
         BNE   *+8                                                              
         SH    R1,=H'6'            LESS IF NO TOTAL                             
         LA    R0,110              MAX IS 110                                   
         SR    R0,R1                                                            
         SRL   R0,1                                                             
         ST    R0,WEEKDISP         DISPLACEMENT=DIFFERENCE/2                    
         CLI   LEFTOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   WEEKDISP+3,0        OPTION NOT TO CENTER                         
         SPACE 1                                                                
         CLI   LINE,50             DON'T START RECAP AT BOTTOM OF PAGE          
         BL    RECAP2                                                           
         MVI   FORCEHED,C'Y'                                                    
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVC   BOXROWS,SPACES                                                   
         B     RECAP4                                                           
         SPACE 1                                                                
RECAP2   BAS   RE,WEEKTITL         SET UP TITLES/BOXES FOR REPORT               
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVC   BOXCOLS(110),WEEKBOX                                             
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         MVI   3(R1),C'M'                                                       
         MVI   BOXROWS+58,C'B'                                                  
         DROP  R5                                                               
         GOTO1 SPOOL,DMCB,(R8)     AND PRINT IN MIDDLE OF PAGE                  
         MVC   P(110),WEEKTIT1                                                  
         MVC   P2(110),WEEKTIT2                                                 
         MVI   SPACING,2                                                        
         BASR  RE,RF                                                            
         SPACE 1                                                                
RECAP4   L     R5,APBUFF           NOW SET UP TO PRINT RECAP                    
         USING PWD,R5                                                           
         LA    R5,L'PWENTRY(R5)                                                 
         LA    R0,200              DO THE DETAILS FIRST                         
         SPACE 1                                                                
RECAP6   BAS   RE,RECFORM                                                       
         LA    R5,L'PWENTRY(R5)                                                 
         CLI   0(R5),0                                                          
         BE    RECAP8                                                           
         BCT   R0,RECAP6                                                        
         SPACE 1                                                                
RECAP8   L     R5,APBUFF           AND THEN THE TOTALS                          
         BAS   RE,RECFORM                                                       
         ZIC   R1,LINE                                                          
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         DROP  R5                                                               
         CLI   ANYMORE,C'Y'        DO WE HAVE ANY MORE WEEKS                    
         BNE   RECAPEND                                                         
         ZIC   R1,LOWWEEK          YES, SO UPDATE LOW WEEK                      
         ZIC   R0,NUMWEEKS                                                      
         AR    R1,R0                                                            
         STC   R1,LOWWEEK                                                       
         B     RECAPB              AND RETURN FOR ANOTHER RECAP                 
         SPACE 1                                                                
RECAPEND MVI   REPTYPE,0                                                        
         MVI   HIGHWEEK,0                                                       
         MVI   LOWWEEK,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT AND PRINT RECAP LINE                           
         SPACE 3                                                                
*              INPUT               R5=A(RECAP ENTRY)                            
         SPACE 1                                                                
RECFORM  NTR1                                                                   
         USING PWD,R5                                                           
         L     R2,WEEKDISP                                                      
         LA    R2,P+1(R2)          DISPLACE INTO PRINT LINE +1                  
         CLI   PWTYPE,1            FOR TOTAL LINE                               
         BNE   RECFORM2                                                         
         GOTO1 SPOOL,DMCB,(R8)     SPACE A LINE BEFORE                          
         MVC   8(6,R2),=C'TOTALS'                                               
         B     RECFORM4                                                         
         SPACE 1                                                                
RECFORM2 MVC   0(16,R2),PWPROG     FOR DETAILS SHOW PROGRAM                     
         EDIT  (1,PWLEN),(3,17(R2)) AND LENGTH                                  
         SPACE 1                                                                
RECFORM4 LA    R2,20(R2)           SET UP TO EDIT WEEKS                         
         LA    R3,PWWEEKS                                                       
         ZIC   R1,LOWWEEK                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         AR    R3,R1               START AT LOWEST WEEK ACTIVE                  
         ZIC   R4,NUMWEEKS                                                      
         SPACE 1                                                                
RECFORM6 EDIT  (2,0(R3)),(3,0(R2))                                              
         LA    R2,4(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,RECFORM6                                                      
         CLI   ANYMORE,C'Y'        THEN SHOW TOTAL UNITS                        
         BE    RECFORM8                                                         
         EDIT  (2,PWTOTAL),(5,0(R2))                                            
         SPACE 1                                                                
RECFORM8 MVC   SPACING,SPACOPT                                                  
         GOTO1 SPOOL,DMCB,(R8)     AND PRINT A LINE                             
         CLI   ANYMORE,C'Y'        IF THIS IS LAST RECAP,                       
         BE    XIT                                                              
         XC    PWENTRY,PWENTRY     CLEAR THIS ENTRY                             
         DROP  R5                                                               
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         NETGO NVTITOUT,DMCB,H1+35                                              
         SPACE 1                                                                
         MVC   H4(6),=C'CLIENT'    CLIENT                                       
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H4+14(20),SPLCLIN                                                
         SPACE 1                                                                
         MVC   H5(7),=C'PRODUCT'   PRODUCT                                      
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H5+14(20),SPLPRON                                                
         SPACE 1                                                                
         MVC   H6(8),=C'ESTIMATE'  ESTIMATE                                     
         EDIT  (1,EFFEST),(3,H6+10),ALIGN=LEFT                                  
         BAS   RE,CHEKEST                                                       
         MVC   H6+14(20),EFFENAME                                               
         SPACE 1                                                                
         CLI   TOTLEVEL,3          NETWORK                                      
         BH    HOOK2                                                            
         MVC   H5+77(9),=C'NETWORK -'                                           
         MVC   H5+87(4),EFFNET                                                  
         SPACE 1                                                                
HOOK2    CLI   TOTLEVEL,2          PACKAGE DETAILS                              
         BH    HOOK4                                                            
         LA    R5,SAVEPACK                                                      
         USING SORTAREA,R5                                                      
         MVC   H5+92(8),SPDP       (DAYPART FROM SAVED PACKAGE)                 
         MVC   H6+77(9),=C'PACKAGE -'                                           
         EDIT  (1,SPACK),(3,H6+87),ALIGN=LEFT                                   
         MVC   H6+92(16),SPNAME                                                 
         DROP  R5                                                               
         SPACE 1                                                                
HOOK4    BAS   RE,HOOKBOX                                                       
         CLI   AVEOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   H6+47(16),=C'(AVERAGE OPTION)'                                   
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - REFRESH ESTIMATE NAME                                     
         SPACE 3                                                                
CHEKEST  NTR1                                                                   
         CLC   EFFEST,SAVEST                                                    
         BE    XIT                                                              
         MVC   SAVEST,EFFEST                                                    
         MVC   EFFENAME,=CL20'NOT FOUND?'                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,NBSELPRD                                                 
         MVC   EKEYEST,EFFEST                                                   
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   EFFENAME,EDESC                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              HOOK - CONTROL OF HEADINGS AND BOXES                             
         SPACE 3                                                                
HOOKBOX  NTR1                                                                   
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVC   BOXYORN,BOXOPT                                                   
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         CLI   REPTYPE,C'R'                                                     
         BE    HOOKREC                                                          
         BAS   RE,DETTITL                                                       
         MVC   BOXCOLS(110),DETBOX                                              
         MVC   H9(110),DETTIT1                                                  
         MVC   H10(110),DETTIT2                                                 
         CLI   REPTYPE,C'T'                                                     
         BNE   XIT                                                              
         MVI   BOXROWS+10,C'B'                                                  
         MVI   BOXROWS+58,C' '                                                  
         B     XIT                                                              
         SPACE 1                                                                
HOOKREC  BAS   RE,WEEKTITL                                                      
         MVC   BOXCOLS(110),WEEKBOX                                             
         MVC   H9(110),WEEKTIT1                                                 
         MVC   H10(110),WEEKTIT2                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO SET UP BOXES AND TITLES FOR DETAIL                   
         SPACE 3                                                                
DETTITL  NTR1                                                                   
         MVC   DETTIT1,SPACES                                                   
         MVC   DETTIT2,SPACES                                                   
         MVC   DETBOX,SPACES                                                    
         SPACE 1                                                                
         LA    R2,DETTIT1                                                       
         LA    R3,DETTIT2                                                       
         LA    R4,DETBOX                                                        
         A     R2,DETDISP                                                       
         A     R3,DETDISP                                                       
         A     R4,DETDISP                                                       
         SPACE 1                                                                
         MVC   0(40,R2),=C' PROGRAM NAME     LEN AVE SHR AVE. UNITS'            
         MVC   0(40,R3),=C'                      HUT     RTG.      '            
         MVC   0(40,R4),=C'L                C   C   C   C    C     '            
         LA    R2,40(R2)                                                        
         LA    R3,40(R3)                                                        
         LA    R4,40(R4)                                                        
         MVC   0(15,R2),=C'      HOMES     '                                    
         MVC   0(15,R3),=C'   IMPS.  GRPS  '                                    
         MVC   0(16,R4),=C'C              R'                                    
         LA    R2,15(R2)                                                        
         LA    R3,15(R3)                                                        
         LA    R4,15(R4)                                                        
         SR    R5,R5                                                            
         ZIC   R0,NUMDEMS                                                       
         LTR   R0,R0                                                            
         BZ    DTIT4                                                            
         SPACE 1                                                                
DTIT2    NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   6(7,R2),WORK        DEMO NAME TO FIRST LINE                      
         MVC   0(18,R3),=C'  VPH  IMPS  GRPS  '                                 
         MVC   0(19,R4),=C'C                 R'                                 
         BAS   RE,DASHES                                                        
         LA    R2,18(R2)                                                        
         LA    R3,18(R3)                                                        
         LA    R4,18(R4)                                                        
         LA    R5,1(R5)                                                         
         BCT   R0,DTIT2                                                         
         SPACE 1                                                                
DTIT4    CLI   REPTYPE,C'T'        ADJUST FOR TOTALS                            
         BNE   ALLBOX                                                           
         LA    R2,DETBOX                                                        
         LA    R3,H9                                                            
         A     R2,DETDISP                                                       
         A     R3,DETDISP                                                       
         MVC   1(33,R2),SPACES                                                  
         MVC   1(33,R3),SPACES                                                  
         LA    R3,132(R3)                                                       
         MVC   1(33,R3),SPACES                                                  
         B     ALLBOX                                                           
         SPACE 1                                                                
DASHES   NTR1                                                                   
         LA    R2,2(R2)                                                         
         LA    R0,15                                                            
         SPACE 1                                                                
DASH2    CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         BCT   R0,DASH2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO SET UP BOXES AND TITLES FOR RECAPS                   
         SPACE 3                                                                
WEEKTITL NTR1                                                                   
         MVC   WEEKTIT1,SPACES                                                  
         MVC   WEEKTIT2,SPACES                                                  
         MVC   WEEKBOX,SPACES                                                   
         LA    R2,WEEKBOX                                                       
         LA    R3,WEEKTIT1                                                      
         LA    R4,WEEKTIT2                                                      
         A     R2,WEEKDISP                                                      
         A     R3,WEEKDISP                                                      
         A     R4,WEEKDISP                                                      
         MVI   0(R2),C'L'                                                       
         MVC   1(12,R3),=C'PROGRAM NAME'                                        
         MVI   17(R2),C'C'                                                      
         MVI   21(R2),C'C'                                                      
         MVC   18(3,R3),=C'LEN'                                                 
         LA    R2,22(R2)           NOW SET UP TO DO THE WEEKS                   
         LA    R3,22(R3)                                                        
         LA    R4,22(R4)                                                        
         ZIC   R5,LOWWEEK                                                       
         BCTR  R5,0                                                             
         SLL   R5,2                                                             
         LA    R5,WLIST(R5)                                                     
         ZIC   R0,NUMWEEKS                                                      
         SPACE 1                                                                
WEEKT2   GOTO1 DATCON,DMCB,(2,0(R5)),(8,DUB)                                    
         MVC   0(3,R3),DUB         MONTH TO LINE 1                              
         MVC   1(2,R4),DUB+3       DAY TO LINE 2                                
         MVI   3(R2),C'C'                                                       
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,WEEKT2                                                        
         CLI   ANYMORE,C'Y'                                                     
         BNE   WEEKT4                                                           
         BCTR  R2,0                                                             
         MVI   0(R2),C'R'                                                       
         B     ALLBOX                                                           
         SPACE 1                                                                
WEEKT4   MVC   0(5,R3),=C'TOTAL'   AND TOTAL                                    
         MVI   5(R2),C'R'                                                       
         SPACE 1                                                                
ALLBOX   L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS AND SPECS                                              
         SPACE 3                                                                
RELOC    DC    A(*)                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=148'                                   
         SPACE 1                                                                
MYBOX    DC    C'Y'                                                             
         DC    AL1(1)                                                           
         DC    6X'00'                                                           
MYCOLS   DC    264C' '                                                          
MYROWS   DC    C'        T                     '                                
         DC    C'                            B '                                
         DC    40C' '                                                           
         DC    28X'00'                                                          
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**SPECS*'                                                      
PBSPECS  SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,36,C'PRE-BUY EVALUATION REPORT'                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,78,AGYNAME                                                    
         SSPEC H2,78,AGYADD                                                     
         SSPEC H4,43,PERIOD                                                     
         SSPEC H4,78,NETREP                                                     
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 1                                                                
SLLIST   DC    AL1(10,15,20,30,40,45,60,90,120)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*              LTORG, NET LIST AND PROGRAM BUFFER                               
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*NETLIST'                                                      
NETLIST  DC    1000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'PROGBUFF'                                                      
PROGBUFF DC    30150X'00'          ALLOW FOR 200 PROGRAMS                       
*                                  PLUS ONE LINE FOR TOTALS                     
*                                  ENTRIES COVERED BY PWD (BELOW)               
         SPACE 1                                                                
*                                  ACCUMULATORS (11X120)                        
*                                  LINE 1 FOR TOTALS                            
*                                  LINE 2-11 FOR SPOT LENGTH ANALYSIS           
PTOTS    DC    1320X'00'           PACKAGE TOTALS                               
NTOTS    DC    1320X'00'           NETWORK TOTALS                               
ATOTS    DC    1320X'00'           ALL NETWORK TOTALS                           
         EJECT                                                                  
*              DSECTS FOR PROGRAM BUFFER AND ACCUMULATOR CHUNK                  
         SPACE 3                                                                
PWD      DSECT                                                                  
PWENTRY  DS    0CL150              DSECT FOR PROGRAM WEEK ENTRY                 
PWTYPE   DS    CL1                 1=TOTALS 2=PROGRAM ENTRY                     
PWPROG   DS    CL16                PROGRAM NAME                                 
PWLEN    DS    XL1                 SECONDS LENGTH                               
         DS    XL2                 SPARE                                        
PWTOTAL  DS    XL2                 TOTAL UNITS                                  
PWWEEKS  DS    XL128               UP TO 64 WEEK SLOTS                          
         SPACE 3                                                                
ACCUMD   DSECT                                                                  
ACENTRY  DS    0CL120              DSECT FOR ACCUMULATOR ENTRY                  
ACUN     DS    F                   UNITS                                        
ACHUT    DS    F                   HUT                                          
ACSHR    DS    F                   SHARE                                        
ACDOL    DS    F                   DOLLARS                                      
         DS    2F                  SPARE                                        
ACHOMVPH DS    F                   (DUMMY)                                      
ACHOMIMP DS    F                   HOMES IMPRESSIONS                            
ACHOMRTG DS    F                   HOMES RATING                                 
ACDEMVPH DS    F                   DEMO (1) VPH                                 
ACDEMIMP DS    F                   DEMO (1) IMPRESSIONS                         
ACDEMRTG DS    F                   DEMO (1) RATINGS                             
         DS    3F                  DEMO (2)                                     
         DS    3F                  DEMO (3)                                     
ACRAW    DS    12F                 AS ABOVE BUT EQUIVALENCED                    
         EJECT                                                                  
*              DSECTS FOR PRE-BUY REPORT                                        
         SPACE 3                                                                
PRED     DSECT                     COMMON WITH EDIT                             
*              NETDEMOD AND                                                     
*              DEDBLOCK HERE                                                    
         PRINT OFF                                                              
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
         PRINT ON                                                               
*                                  FIELDS SHARED WITH EDIT                      
SPACOPT  DS    CL1                                                              
SEPOPT   DS    CL1                                                              
BOXOPT   DS    CL1                                                              
LEFTOPT  DS    CL1                                                              
AVEOPT   DS    CL1                                                              
PFBOPT   DS    CL1                                                              
ACTOPT   DS    CL1                                                              
         SPACE 1                                                                
*                                  LOCAL WORKING STORAGE                        
         SPACE 1                                                                
APBUFF   DS    A                                                                
RELO     DS    A                                                                
PERTYPE  DS    CL3                                                              
WLIST    DS    CL256               64 4-BYTE WEEKS                              
WLISTSP  DS    CL4                 + SPARE                                      
ANYSORT  DS    CL1                                                              
FIRSTPAK DS    CL1                 FIRST TIME INDIC FOR PACKAGE                 
TOTLEVEL DS    XL1                 TOTAL LEVEL 1-4                              
SAVIMPE  DS    CL1                 SAVE IMPRESSION EQUIVALENCY                  
SAVGRPE  DS    CL1                 SAVE RATING EQUIVALENCY                      
SAVER2   DS    A                                                                
SAVELEN  DS    XL1                                                              
NLINES   DS    XL1                                                              
HIGHWEEK DS    XL1                 HIGHEST WEEK NUMBER FOR PACKAGE              
LOWWEEK  DS    XL1                 LOWEST WEEK NUMBER                           
NUMWEEKS DS    XL1                 NUMBER OF WEEKS                              
ANYMORE  DS    CL1                 WEEK SWITCH                                  
NUMDEMS  DS    XL1                 NUMBER OF DEMOS FOR REPORTING                
REPTYPE  DS    CL1                 (C'R'=PROCESSING RECAP)                      
WEEKDISP DS    F                   DISPLACEMENT INTO WEEK LINE                  
DETDISP  DS    F                   DISPLACEMENT INTO DETAIL LINE                
EFFEST   DS    XL1                 EFFECTIVE ESTIMATE NUMBER                    
SAVEST   DS    XL1                 NUMBER READ                                  
EFFENAME DS    CL20                AND IT'S NAME                                
EFFNET   DS    CL4                 EFFECTIVE NETWORK                            
AVESW    DS    CL1                 SWITCHES FOR FORMAT ROUTINE                  
EQUSW    DS    CL1                                                              
CPMSW    DS    CL1                                                              
SAVPCOST DS    F                   SAVED PACKAGE COST                           
COST     DS    F                   SAVED COST FOR FORMAT                        
UNITS    DS    F                   SAVED UNITS FOR FORMAT                       
P1AREA   DS    CL16                                                             
P3AREA   DS    CL16                                                             
SURVEY   DS    CL1                 SAVED SURVEY                                 
         DS    0D                                                               
         SPACE 1                                                                
SAVEPACK DS    CL148                                                            
LASTREC  DS    CL148                                                            
SORTAREA DS    0CL148              SORT RECORD AREA                             
SEST     DS    XL1                 ESTIMATE NUMBER                              
SNET     DS    CL4                 NETWORK                                      
SPACK    DS    XL1                 PACKAGE NUMBER                               
STYPE    DS    XL1                 TYPE 1=PACKAGE 2=UNIT                        
         DS    XL1                 SPARE                                        
         SPACE 1                                                                
*                                  PACKAGE RECORDS                              
SPNAME   DS    CL16                PACKAGE NAME                                 
SPCOST   DS    XL4                 PACKAGE COST                                 
SPGCPM   DS    XL4                 GUARANTEED CPM                               
SPDP     DS    CL8                 PACKAGE DAYPART                              
         SPACE 1                                                                
*                                  UNIT RECORDS                                 
         ORG   SPNAME                                                           
SPROG    DS    CL16                PROGRAM NAME                                 
SLEN     DS    XL1                 SECONDS LENGTH                               
SWEEK    DS    XL1                 WEEK NUMBER                                  
SSURVEY  DS    XL1                 SURVEY N/C                                   
SLENNO   DS    XL1                 SPOT LENGTH NUMBER                           
SACCUMS  DS    XL120               ACCUMULATORS                                 
         SPACE 2                                                                
LTOTS    DS    XL120               PROGRAM/LENGTH TOTALS                        
         SPACE 1                                                                
WEEKTIT1 DS    CL110               TITLE LINE 1 FOR RECAP                       
WEEKTIT2 DS    CL110               TITLE LINE 2 FOR RECAP                       
DETTIT1  DS    CL110               TITLE LINE 1 FOR DETAIL                      
DETTIT2  DS    CL110               TITLE LINE 2 FOR DETAIL                      
WEEKBOX  DS    CL110               BOX COLUMNS FOR RECAP                        
DETBOX   DS    CL110               BOX COLUMNS FOR DETAILS                      
         DS    0F                                                               
*              SPGENEST            INCLUDES HERE                                
*              NEGENPACK                                                        
*              NETINCLS                                                         
*              DDBIGBOX                                                         
*              NEWRIFFD                                                         
*              NEWRIFAD                                                         
         PRINT OFF                                                              
DUMMYD   DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIFAD                                                       
         SPACE 1                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096NEWRI2A   05/01/02'                                      
         END                                                                    

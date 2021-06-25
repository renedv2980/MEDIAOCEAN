*          DATA SET NEMED6BT   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED6B    AT LEVEL 083 AS OF 02/23/99                      
*PHASE T31E6BA,+0                                                               
         TITLE 'T31E6B - THREE IN ONE REPORT'                                   
T31E6B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTTH**,RA,RR=R2                                              
         USING T31E6B+4096,RA                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     RF,=A(NETTBL)       MOVE NBANBUFF TO TABLE                       
         AR    RF,R2               ADD RELO                                     
         ST    RF,NBANBUFF         FORMERLY USED ANETWS1 BUT TOO SMALL          
         L     RE,ANETWS2          NBANBUFF FROM EDIT MODULE                    
         LA    R1,4000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RE,ANETWS2          CLEAR THE AREA                               
         LA    RF,4000                                                          
         XCEF                                                                   
NOTBL    EQU   *                                                                
         L     R7,ANETWS1           W1,W2,W3,W4 ETC NOW WORKAREA                
         USING POSTD,R7                                                         
         ST    R2,RELO                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVI   TOTFLAG,0                                                        
         LA    R2,ESTAC                                                         
         A     R2,=F'1088'                                                      
         XC    0(160,R2),0(R2)     CLEAR NDOLTBL                                
*                                                                               
         MVC   MENU,NBUSER+9                                                    
         CLI   MENU,0                                                           
         BNE   *+8                                                              
         MVI   MENU,1                                                           
         CLI   MENU,12                                                          
         BNH   *+8                                                              
         MVI   MENU,1                                                           
*                                                                               
         MVC   PERTYPE(1),DOPT     WEEKS OR MONTHS                              
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         MVI   PERTYPE+2,0         DONT USE QUARTERS                            
         EJECT                                                                  
*              CONTROL OF SUMMARIES                                             
*                                                                               
         MVI   NBHUNOPT,C'Y'       REPORT HANDLES HUNDREDS                      
         CLI   DETAIL,C'P'         IF DETAIL IS P, USE SEQ SET IN EDIT          
         BE    IN2                                                              
         MVI   NBSEQ,C'N'          ELSE READ BY NETWORK,DATE,ESTMATE            
*                                                                               
IN2      CLI   FLAVOR,C'E'         ESTIMATE FLAVOR                              
         BE    INFLAVE                                                          
         CLI   FLAVOR,C'V'         ESTIMATED DEMOS FLAVOR                       
         BE    INFLAVV                                                          
         CLI   FLAVOR,C'P'         POST FLAVOR                                  
         BE    INFLAVP                                                          
         DC    H'0'                                                             
INFLAVE  MVI   RCSUBPRG,3          FLAVOR E                                     
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBUSER+13,C'N'      DONT FILTER PRE-EMPTS                        
         MVI   NBUSER+8,C'Y'       PUT ASSIGNED COST IN NBCALCOS                
         MVI   NREPTYP,C'A'        ACCOUNTING REPORT                            
         B     DOPKG                                                            
INFLAVV  MVI   RCSUBPRG,2          FLAVOR V                                     
         MVI   NBDATA,C'B'         PACKAGES AND UNITS                           
         MVI   NBESTOPT,C'Y'       GET ESTIMATED DEMOS                          
         MVI   NBSELUOP,C'E'       USE EST SCHEDULE                             
         B     DOPKG                                                            
INFLAVP  MVI   RCSUBPRG,1          FLAVOR P                                     
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBESTOPT,C'Y'       GET ESTIMATED DEMOS                          
         MVI   NBACTOPT,C'Y'       AND ACTUAL DEMOS                             
         B     DOPKG                                                            
*                                                                               
DOPKG    CLI   NBMODE,NBPROCPK     FOR SINGLE PACKAGE READ, CURRENT             
         BE    PROCPAK              RECORD IS ALREADY A PACKAGE                 
GETPACK  NETGO NSNETIO,DMCB,NETBLOCK    PROCESS PACKAGES AND DATES 1ST          
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCPK     IF A PACKAGE READ                            
         BE    PROCPAK                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    GOTDAT                                                           
         B     GETPACK                                                          
*                                                                               
*                             SETS PKG DOLLARS INTO TABLE BY NTWK               
PROCPAK  CLI   FLAVOR,C'V'                                                      
         BNE   GETPACK             IF FLAVOR NOTV THEN IGNORE PACKAGES          
         LA    R2,ESTAC            GET ADDRESSABILITY TO TABLE                  
         A     R2,=F'1088'         R2-NDOLTBL                                   
         LA    R0,20               MAX NUM OF NTWKS                             
PP2      OC    0(4,R2),0(R2)                                                    
         BZ    PP5                                                              
         CLC   0(4,R2),NBACTNET                                                 
         BE    PP5                                                              
         LA    R2,8(R2)                                                         
         BCT   R0,PP2                                                           
         DC    H'0'                                                             
PP5      MVC   0(4,R2),NBACTNET                                                 
         L     R1,4(R2)                                                         
         A     R1,NBPAKCST                                                      
         ST    R1,4(R2)                                                         
         LA    R2,ESTAC                                                         
*        A     R2,=F'1248'          ADD TO REPTOT                               
*        L     R3,0(R2)            ADD TO REPORT TOTAL                          
*        A     R3,NBPAKCST                                                      
*        ST    R3,0(R2)                                                         
         B     GETPACK                                                          
*        B     GETPACK                                                          
*                                                                               
*                                                                               
GOTDAT   LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS                                                       
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE   FILL LIST                 
         MVC   SAVENET,NBACTNET    LAST PKG MAY HAVE SAME NET AS UNIT           
         MVI   FRSTUNIT,C'Y'                                                    
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT READ                               
         BE    GOTUN                                                            
         CLI   NBMODE,NBREQLST     IF LAST RECORD READ                          
         BE    LASTONE                                                          
         B     GETUNIT                                                          
*                                                                               
GOTUN    CLI   FRSTUNIT,C'Y'                                                    
         BNE   GOTUN2                                                           
         MVI   FRSTUNIT,0                                                       
         MVC   SAVEPROG,NBPROGNM                                                
         MVC   SAVEEST,NBACTEST                                                 
         MVC   SAVEPAK,NBPACK                                                   
         B     GOTUN5                                                           
GOTUN2   TM    NBSUBMSK,NBSBMPRG   IF A NEW PROGRAM                             
         BZ    CKNET                                                            
         BAS   RE,PROGTOTS                                                      
         MVC   SAVEPROG,NBPROGNM                                                
         MVC   SAVEEST,NBACTEST                                                 
         MVC   SAVEPAK,NBPACK                                                   
CKNET    TM    NBSUBMSK,NBSBMNET   IF A NEW NETWORK                             
         BZ    DOUNIT                                                           
         BAS   RE,ESTTOTS                                                       
         BAS   RE,NETTOTS                                                       
GOTUN5   CLI   FLAVOR,C'V'                                                      
         BNE   *+8                                                              
         BAS   RE,SETACDOL                                                      
         MVC   SAVENET,NBACTNET                                                 
         CLI   DETAIL,C'N'                                                      
         BE    DOUNIT                                                           
         MVI   FORCEHED,C'Y'                                                    
DOUNIT   BAS   RE,POST             FOR EVERY UNIT                               
         B     GETUNIT                                                          
*                                                                               
LASTONE  BAS   RE,PROGTOTS                                                      
         MVI   TOTFLAG,1           PRINT CPM LINE                               
*        CLI   FLAVOR,C'V'                                                      
*        BNE   *+8                                                              
*        BAS   RE,SETACDOL                                                      
         BAS   RE,NETTOTS                                                       
         BAS   RE,ESTTOTS                                                       
         BAS   RE,PRODTOTS                                                      
         XIT1                                                                   
*                                                                               
PROCERR  DC    H'0'                                                             
*                                                                               
*                                                                               
*                       LOADS ACDOL WITH PKG COST FROM NTWK/PKGTBL              
*                       - ASSUMES REPORT IN NTWK ORDER                          
SETACDOL NTR1                                                                   
         LA    R3,ESTAC                                                         
         A     R3,=F'1088'      R3-R3-NDOLTBL                                   
SET4     CLC   NBACTNET,0(R3)      PKG=UNIT(CAN BE PKG AND NO UNITS)            
         BE    SET6                                                             
         OC    0(8,R3),0(R3)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(160,R3),8(R3)                                                  
         B     SET4                                                             
SET6     LA    R2,NETAC+1024                                                    
         USING ACCUMD,R2                                                        
         L     R1,4(R3)                                                         
         A     R1,ACDOL                                                         
         ST    R1,ACDOL                                                         
         LA    R2,ESTAC                                                         
         LA    R2,1024(R2)         ESTAC+1024                                   
         L     R1,4(R3)                                                         
         A     R1,ACDOL                                                         
         ST    R1,ACDOL                                                         
         CLI   DETAIL,C'E'                                                      
         BNE   SETX                                                             
         LA    R2,PROGAC+1024                                                   
         L     R1,4(R3)                                                         
         A     R1,ACDOL                                                         
         ST    R1,ACDOL                                                         
SETX     MVC   0(160,R3),8(R3)     SET FOLLOWING FIELD TO FIRST                 
         B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*              POSTING ROUTINES                                                 
*                                                                               
POST     NTR1                                                                   
         LA    R2,WORKAC                                                        
         XC    WORKAC,WORKAC                                                    
         USING ACCUMD,R2                                                        
*                                                                               
         L     R1,NBASSIGN         SAVE ASSIGNED COST IN DOLLARS                
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,ASSDOLS                                                       
         L     R1,NBACTUAL         SAVE ACTUAL COST IN DOLLARS                  
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,ACTDOLS                                                       
*                                                                               
         CLI   FLAVOR,C'P'         POST                                         
         BNE   POST4                                                            
*                                                                               
         TM    NBUNITST,X'01'      DONT POST MAKEGOODS INTO ESTIMATED           
         BO    POSTACT                                                          
         L     R1,NBINTEG                    ESTIMATED                          
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         CLI   NBUSER+15,C'Y'              (OPTIONALLY + INTEG)                 
         BE    *+6                                                              
         SR    R1,R1                                                            
         A     R1,ACTDOLS          ACTUAL COST IN DOLLARS                       
         ST    R1,ACCOST                                                        
         OC    ACUNITS,ACUNITS                                                  
         MVC   ACUNITS+2(2),NBESTUN                                             
         MVC   ACGRPS+2(2),NBESTHOM+2                                           
         MVC   ACHOMES,NBESTHOM+4                                               
*                                                                               
**** GET DEMOS INTO ACDEMA, ACDEMB                                              
         NETGO NVGETDEM,DMCB,(0,NDDEMBLK),(C'E',ACDEMA)    1ST EST DEMO         
         NETGO NVGETDEM,DMCB,(1,NDDEMBLK),(C'E',ACDEMB)    2ND EST DEMO         
*                                                                               
POSTACT  TM    NBUNITST,X'02'      DONT POST MISSED SPOTS INTO ACTUALS          
         BO    POST2                                                            
         LA    R2,WORKAC+24                  ACTUAL                             
         L     R1,NBINTEG                                                       
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         CLI   NBUSER+15,C'Y'                                                   
         BE    *+6                                                              
         SR    R1,R1                                                            
         A     R1,ACTDOLS          ACTUAL COST IN DOLLARS                       
         ST    R1,ACCOST                                                        
         OC    ACUNITS,ACUNITS                                                  
         MVC   ACUNITS+2(2),NBACTUN                                             
         MVC   ACGRPS+2(2),NBACTHOM+2                                           
         MVC   ACHOMES,NBACTHOM+4                                               
*                                                                               
**** GET DEMOS INTO ACDEMA, ACDEMB                                              
         NETGO NVGETDEM,DMCB,(0,NDDEMBLK),(C'A',ACDEMA)    1ST ACT DEMO         
         NETGO NVGETDEM,DMCB,(1,NDDEMBLK),(C'A',ACDEMB)    2ND ACT DEMO         
*                                                                               
         CLI   NBUSER+8,C'Y'       OPTION TO USE ASSIGNED COSTS                 
         BNE   POST2                                                            
         MVC   WORKAC(4),ASSDOLS   ASSIGNED COST (DOLLARS)                      
         MVC   WORKAC+24(4),ASSDOLS                                             
*                                                                               
POST2    BAS   RE,ADDAC                                                         
         B     XITPOST                                                          
*                                                                               
POST4    CLI   FLAVOR,C'E'         ESTIMATES                                    
         BNE   POST6                                                            
         NETGO NVACFLT,DMCB        ACCOUNTING FILTER                            
         BZ    POST4A                                                           
         MVC   P,SPACES            FILTER. PASS THIS UNIT. RESET P              
         XC    SPOTAC,SPOTAC       RESET SPOTAC                                 
         B     XITPOST                                                          
POST4A   LA    R0,1                1 UNIT                                       
         ST    R0,ACUNE                                                         
         MVC   ACASS,ASSDOLS       ASSIGNED COST IN DOLLARS                     
         MVC   ACACT,ACTDOLS       ACTUAL COST IN DOLLARS                       
         MVC   ACINT,NBINTEG                                                    
*        MVC   ACCUT,SPOTACUT*********???? CUT IN????? ***********              
         CLI   FILTER,C'1'                                                      
         BL    POST5                                                            
         XC    SPOTAC+16(4),SPOTAC+16        NO CUT INS                         
*                                                                               
POST5    BAS   RE,ADDAC                                                         
         B     XITPOST                                                          
         SPACE 2                                                                
POST6    OC    ACUN,ACUN                                                        
         MVC   ACUN+2(2),NBESTUN   EVALUATION                                   
         MVC   ACHUT+2(2),NBESTHUT                                              
         MVC   ACSHR+2(2),NBESTSHR                                              
         LA    R4,ACHOMVPH         DO HOMES                                     
         LA    R5,NBESTHOM                                                      
         MVC   2(2,R4),0(R5)       HOME VPHS                                    
         MVC   6(2,R4),2(R5)       HOME POINTS                                  
         MVC   8(4,R4),4(R5)       HOME IMPS                                    
*                                                                               
         ZIC   R3,NDNDEMOS                                                      
         CH    R3,=H'3'                                                         
         BL    *+8                                                              
         LA    R3,3                                                             
         LTR   R3,R3               SKIP IF 0 DEMOS                              
         BZ    POST10                                                           
         LA    R5,NDESTDEM                                                      
         LA    R4,12(R4)           NOW OTHER DEMOS                              
POST8    MVC   2(2,R4),0(R5)       VPH                                          
         MVC   6(2,R4),2(R5)       POINTS                                       
         MVC   8(4,R4),4(R5)       IMPS                                         
         LA    R5,8(R5)                                                         
         LA    R4,12(R4)                                                        
         BCT   R3,POST8                                                         
POST10   BAS   RE,ADDAC                                                         
XITPOST  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD INTO ACCUMULATOR ARRAY                            
*                                                                               
ADDAC    NTR1                                                                   
         SR    R5,R5                                                            
         LA    R2,MONLIST          ESTABLISH DATE ANALYSIS NUMBER               
ADLOOP   LA    R5,1(R5)                IN R5                                    
         CLC   NBACTDAT(2),2(R2)                                                
         BNH   ADDAT                                                            
         LA    R2,4(R2)                                                         
         B     ADLOOP                                                           
*                                                                               
ADDAT    BCTR  R5,0                                                             
         LA    R2,WORKAC                                                        
         LA    R4,16                                                            
         SLL   R5,6                                                             
         LA    R3,PROGAC                                                        
         BAS   RE,ADD2                                                          
         LA    R3,NETAC                                                         
         BAS   RE,ADD2                                                          
         LA    R3,ESTAC                                                         
         BAS   RE,ADD2                                                          
         B     XIT                                                              
         SPACE 2                                                                
ADD2     NTR1                                                                   
         LA    R3,1024(R3)         DISPLACE TO TOTAL LINE                       
         BAS   RE,ADD4             ADD INTO THAT                                
         SH    R3,=H'1024'                                                      
         AR    R3,R5               DISPLACE TO DATE LINE                        
         BAS   RE,ADD4             ADD INTO THAT                                
         B     XIT                                                              
         SPACE 2                                                                
ADD4     NTR1                                                                   
         SPACE 2                                                                
ADD6     L     R1,0(R2)            ROUTINE ADDS LINE (R2) TO (R3)               
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADD6                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL OF TOTALS                                                
         SPACE 3                                                                
PROGTOTS NTR1                                                                   
         LA    R2,PROGAC                                                        
         CLI   DETAIL,C'E'                                                      
         BE    XIT                                                              
         CLI   DETAIL,C'P'                                                      
         BNE   PROG2                                                            
         MVI   TOTFLAG,C'N'        DONT USE CPM ON TOTAL LINE                   
         MVC   P(16),SAVEPROG                                                   
         BAS   RE,FORMEPL                                                       
         BAS   RE,CHUNK                                                         
         B     PROG2                                                            
         SPACE 2                                                                
ESTTOTS  NTR1                                                                   
         CLI   DETAIL,C'E'                                                      
         BNE   XIT                                                              
         LA    R2,PROGAC                                                        
         MVI   TOTFLAG,C'Y'        USE CPM ON TOTAL LINE                        
         MVC   P+4(8),=C'ESTIMATE'                                              
         EDIT  (1,NBACTEST),(3,P+13),ALIGN=LEFT                                 
         BAS   RE,CHUNK                                                         
         SPACE 2                                                                
PROG2    BAS   RE,CLEAR                                                         
         B     XIT                                                              
         SPACE 2                                                                
NETTOTS  NTR1                                                                   
         LA    R2,NETAC                                                         
         MVI   TOTFLAG,C'Y'        USE CPM ON TOTAL LINE                        
         MVC   P+8(4),SAVENET                                                   
         BAS   RE,CHUNK                                                         
         B     PROG2                                                            
         SPACE 2                                                                
PRODTOTS NTR1                                                                   
         LA    R2,ESTAC                                                         
         MVI   TOTFLAG,C'Y'        USE CPM ON TOTAL LINE                        
         MVC   P+6(6),=C'TOTALS'                                                
         BAS   RE,CHUNK                                                         
         B     PROG2                                                            
         SPACE 2                                                                
CLEAR    NTR1                                                                   
         MVC   P,SPACES                                                         
         LA    R3,17                                                            
         SPACE 2                                                                
CLEAR2   XC    0(64,R2),0(R2)                                                   
         LA    R2,64(R2)                                                        
         BCT   R3,CLEAR2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A CHUNK                                         
         SPACE 3                                                                
CHUNK    NTR1                                                                   
         LR    R3,R2                                                            
         SR    R4,R4                                                            
         LA    R5,16                                                            
         MVI   OPTION,C'N'                                                      
         MVI   VPHSW,C'Y'                                                       
         SPACE 2                                                                
CHUNK2   OC    0(64,R3),0(R3)      FIRST COUNT ACTIVE LINES                     
         BZ    *+8                                                              
         LA    R4,1(R4)                                                         
         LA    R3,64(R3)                                                        
         BCT   R5,CHUNK2                                                        
         LTR   R4,R4                                                            
         BZ    CHUNK10                                                          
         LA    R4,4(R4)                                                         
         STC   R4,ALLOWLIN                                                      
         LA    R3,MONLIST                                                       
         LA    R4,16                                                            
         SPACE 2                                                                
CHUNK4   OC    0(64,R2),0(R2)                                                   
         BZ    CHUNK8                                                           
         GOTO1 DATCON,DMCB,(2,(R3)),(4,P+17)                                    
         MVI   P+22,C'-'                                                        
         GOTO1 DATCON,DMCB,(2,2(R3)),(4,P+23)                                   
         SPACE 2                                                                
CHUNK6   BAS   RE,FORMAT                                                        
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(16),EPL                                                        
         MVC   EPL,SPACES                                                       
         MVI   VPHSW,C'N'          DONT REPEAT VPH                              
         SPACE 2                                                                
CHUNK8   LA    R2,64(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R4,CHUNK4                                                        
         SPACE 2                                                                
         MVC   P+20(5),=C'TOTAL'                                                
         MVI   OPTION,C'Y'                                                      
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
CHUNK10  MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT (FORMERLY EST-PAK-LIN. NOW DUMMY))             
         SPACE 3                                                                
FORMEPL  NTR1                                                                   
         CLI   DETAIL,C'P'                                                      
         BE    XITFPL                                                           
         MVC   EPL,SPACES                                                       
         LA    R2,EPL+3                                                         
         MVI   0(R2),C'('                                                       
         EDIT  (1,SAVEEST),(3,1(R2)),ALIGN=LEFT                                 
         AR    R2,R0                                                            
         MVI   1(R2),C'-'                                                       
         LA    R2,2(R2)                                                         
         EDIT  (1,SAVEPAK),(3,0(R2)),ALIGN=LEFT                                 
         AR    R2,R0                                                            
         MVI   0(R2),C')'                                                       
XITFPL   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT PRINT LINES - POSTS                            
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         USING ACCUMD,R2                                                        
         CLI   FLAVOR,C'P'                                                      
         BNE   FORMATE                                                          
         LA    R4,P                                                             
         BAS   RE,PDIFF                                                         
         LA    R4,P+28                                                          
         BAS   RE,PF2                        ESTIMATED                          
         LA    R2,24(R2)                                                        
         LA    R4,41(R4)                                                        
         BAS   RE,PF2                        ACTUAL                             
         B     XIT                                                              
         SPACE 2                                                                
PF2      NTR1                                                                   
         EDIT  (4,ACCOST),(9,0(R4))          COST                               
         EDIT  (4,ACUNITS),(4,10(R4))        UNITS                              
         CLI   10(R4),C' '                                                      
         BNE   *+10                                                             
         MVC   10(4,R4),11(R4)                                                  
         EDIT  (4,ACGRPS),(7,13(R4)),1,ZERO=BLANK                               
         CLI   13(R4),C' '                                                      
         BE    PF4                           GRPS (1 DEC IF ROOM)               
         L     R1,ACGRPS                                                        
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,13(R4))                                                  
         SPACE 2                                                                
PF4      CLI   OPTION,C'Y'                   OPTIONAL CPP BELOW                 
         BNE   PF6                                                              
         L     R1,ACCOST                                                        
         M     R0,=F'20'                                                        
         OC    ACGRPS,ACGRPS                                                    
         BZ    PF6                                                              
         D     R0,ACGRPS                                                        
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(5,147(R4)),FLOAT=$                                         
         SPACE 2                                                                
PF6      LA    R5,ACHOMES                                                       
         LA    R6,20(R4)                                                        
         LA    R3,3                                                             
         SPACE 2                                                                
PF8      NETGO NVPRDEM,DMCB,(C'I',0),(R5),(R6)      IMPS                        
         CLI   0(R6),C' '                    (000) IF SPACE                     
         BE    PF10                                                             
         L     R1,0(R5)                     OTHERWISE SHOW MILLIONS             
         A     R1,=F'5000'                                                      
         SR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         EDIT  (R1),(6,0(R6))                                                   
         MVI   6(R6),C'M'                    DESIGNATED WITH AN 'M'             
         SPACE 2                                                                
PF10     CLI   OPTION,C'Y'                   OPTION TO SHOW CPM BELOW           
         BNE   PF14                                                             
         OC    0(4,R5),0(R5)                                                    
         BZ    PF14                                                             
         L     R1,ACCOST                                                        
         M     R0,=F'2000'                                                      
         D     R0,0(R5)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,133(R6)),2,FLOAT=$                                       
         SPACE 2                                                                
PF14     LA    R5,4(R5)                                                         
         LA    R6,7(R6)                                                         
         BCT   R3,PF8                                                           
         LA    R5,ACDEMA           SUPPORT GRPS                                 
         LA    R6,27(R4)                                                        
         NETGO NVDEMTYP,DMCB,(0,NDDEMBLK),BYTE                                  
         CLI   BYTE,C'R'           MODIFIER OF 1ST DEMO IN BYTE                 
         BNE   *+8                                                              
         BAS   RE,PF16                                                          
         LA    R5,ACDEMB                                                        
         LA    R6,7(R6)                                                         
         NETGO NVDEMTYP,DMCB,(1,NDDEMBLK),BYTE                                  
         CLI   BYTE,C'R'           MODIFIER OF 2ND DEMO IN BYTE                 
         BNE   *+8                                                              
         BAS   RE,PF16                                                          
         B     XIT                                                              
         SPACE 2                                                                
PF16     EDIT  (4,0(R5)),(7,0(R6)),1,ZERO=BLANK                                 
         CLI   0(R6),C' '                                                       
         BE    PF18                                                             
         L     R1,0(R5)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,0(R6))                                                   
         SPACE 2                                                                
PF18     CLI   OPTION,C'Y'         OPTIONAL CPP                                 
         BNE   PF20                                                             
         L     R1,ACCOST                                                        
         M     R0,=F'20'                                                        
         OC    0(4,R5),0(R5)                                                    
         BZ    PF20                                                             
         D     R0,0(R5)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,133(R6)),FLOAT=$                                         
         SPACE 2                                                                
PF20     BR    RE                                                               
PDIFF    NTR1                                                                   
         CLI   OPTION,C'Y'                                                      
         BNE   XIT                                                              
         LA    R5,ACHOMES                    ROUTINE TO SHOW PERCENTAGE         
         LA    R6,92+264(R4)                 DIFFERENCE FOR (UP TO)             
         LA    R3,3                          HOMES AND 2 DEMO CPMS.             
         SPACE 2                                                                
PDIFF2   BAS   RE,PDIFF4                                                        
         LA    R5,4(R5)                                                         
         LA    R6,7(R6)                                                         
         BCT   R3,PDIFF2                                                        
         B     XIT                                                              
         SPACE 2                                                                
PDIFF4   NTR1                                                                   
         L     R1,ACCOST           WORK OUT EST CPM                             
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         OC    0(4,R5),0(R5)                                                    
         BZ    XIT                                                              
         M     R0,=F'2000'                                                      
         D     R0,0(R5)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R3,R1               AND SAVE IN R3                               
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
         L     R1,ACCOST+24        WORK OUT ACTUAL CPM                          
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         OC    24(4,R5),24(R5)                                                  
         BZ    XIT                                                              
         M     R0,=F'2000'                                                      
         D     R0,24(R5)                                                        
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         M     R2,=F'200'          EXPRESS AS INDEX ESTIMATE/ACTUAL             
         DR    R2,R1                                                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         EDIT  (R3),(4,0(R6))                                                   
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO FORMAT PRINT LINE - ESTIMATES                         
         SPACE 3                                                                
FORMATE  CLI   FLAVOR,C'E'                                                      
         BNE   FORM3                                                            
         LA    R4,P                                                             
         EDIT  (4,ACUN),(4,48(R4))                                              
         LA    R3,53(R4)                                                        
         BAS   RE,SOFTCOL                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT PRINT LINES - EVALUATION                       
         SPACE 3                                                                
FORM3    LR    R6,R2                                                            
         DROP  R2                                                               
         USING ACCUMD,R6                                                        
         CLI   TOTFLAG,C'Y'        ONLY PRINT ON PROGRAM LINE                   
         BE    FORM3B                                                           
         L     R1,ACHUT            AVERAGE HUT                                  
         LTR   R1,R1                                                            
         BZ    FORM3A                                                           
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,P+30)                                                    
         MVI   P+32,C' '                                                        
FORM3A   L     R1,ACSHR            SHARE                                        
         LTR   R1,R1                                                            
         BZ    FORM3AA                                                          
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,P+33)                                                    
         MVI   P+35,C' '                                                        
FORM3AA  L     R0,ACHOMRTG         AVE RTG                                      
         LTR   R0,R0                                                            
         BZ    FORM3B                                                           
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,P+36),1                                                  
         SPACE 2                                                                
FORM3B   XC    TOTDISP,TOTDISP                                                  
         MVI   CPMDISP,X'FF'                                                    
         MVI   AVEDISP,X'FF'                                                    
         CLI   FLAVOR+1,C'A'        USE AVEDISP FOR FLAVOR VA ON                
         BNE   FB4                    PROGRAM LINE                              
         CLI   TOTFLAG,C'Y'                                                     
         BE    FB4                                                              
         CLI   OPTION,C'Y'                                                      
         BE    FB4                                                              
         MVI   AVEDISP,0                                                        
FB4      CLI   OPTION,C'Y'         SET CPMDISP IF TOTAL LINE FOR                
         BNE   FB8                   NON PROGRAM                                
         CLI   TOTFLAG,C'Y'                                                     
         BNE   FB8                                                              
         MVC   CPMDISP,=F'132'                                                  
FB8      EDIT  (4,ACUN),(4,P+40)   UNITS                                        
*                                                                               
         CLI   CPMDISP,X'FF'       SHOW TOTAL COST                              
         BE    FORM3D                                                           
         EDIT  (4,ACDOL),(13,P+159),COMMAS=YES,FLOAT=$                          
         SPACE 2                                                                
FORM3D   LA    R2,ACHOMVPH                                                      
         LA    R3,P+43                                                          
         ZIC   R4,NDNDEMOS                                                      
         CH    R4,=H'3'                                                         
         BL    *+8                                                              
         LA    R4,3                                                             
         LA    R4,1(R4)                                                         
         SPACE 2                                                                
FORM32   CLI   TOTFLAG,C'Y'        VPH                                          
         BE    FORM34              ONLY ON PROGRAM LINE                         
         OC    0(4,R2),0(R2)                                                    
         BZ    FORM34                                                           
         CLI   VPHSW,C'Y'                                                       
         BNE   FORM34                                                           
         L     R0,0(R2)                                                         
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BCTR  R3,0                                                             
         EDIT  (R1),(4,0(R3))                                                   
         MVI   3(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         SPACE 2                                                                
FORM34   L     R5,8(R2)            IMPS                                         
         LTR   R5,R5                                                            
         BZ    FORM36                                                           
         CLI   TOTDISP,X'FF'                                                    
         BE    FORM35                                                           
         A     R3,TOTDISP                                                       
         NETGO NVPRDEM,DMCB,(C'I',0),8(R2),2(R3)                                
         CLI   2(R3),C' '                                                       
         BE    FORM34B                                                          
         L     R1,8(R2)            NOT ROOM FOR IMPS                            
         A     R1,=F'5000'         SO SHOW MILLIONS INSTEAD                     
         SR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         EDIT  (R1),(6,2(R3))                                                   
         MVI   8(R3),C'M'          DESIGNATE THESE WITH AN 'M'                  
         SPACE 2                                                                
FORM34B  S     R3,TOTDISP                                                       
         SPACE 2                                                                
FORM35   CLI   AVEDISP,X'FF'                                                    
         BE    FORM36                                                           
         L     R0,8(R2)                                                         
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LA    R1,5(R1)            ROUND TO NEAREST 10000                       
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         ST    R1,IMPS                                                          
         A     R3,AVEDISP                                                       
         NETGO NVPRDEM,DMCB,(C'I',0),IMPS,2(R3)                                 
         CLI   2(R3),C' '                                                       
         BE    FORM35B                                                          
         L     R1,IMPS             NOT ROOM FOR IMPS                            
         A     R1,=F'5000'         SO SHOW MILLIONS INSTEAD                     
         SR    R0,R0                                                            
         D     R0,=F'10000'                                                     
         EDIT  (R1),(6,2(R3))                                                   
         MVI   8(R3),C'M'          DESIGNATE THESE WITH AN 'M'                  
         SPACE 2                                                                
FORM35B  S     R3,AVEDISP                                                       
         SPACE 2                                                                
FORM36   L     R5,4(R2)            GRPS                                         
         LTR   R5,R5                                                            
         BZ    FORM40                                                           
         CH    R5,=H'9999'                                                      
         BH    FORM38                                                           
         CLI   TOTDISP,X'FF'                                                    
         BE    FORM36B                                                          
         A     R3,TOTDISP                                                       
         EDIT  (R5),(5,10(R3)),1                                                
         S     R3,TOTDISP                                                       
         SPACE 2                                                                
FORM36B  CLI   AVEDISP,X'FF'                                                    
         BE    FORM40                                                           
         A     R3,AVEDISP                                                       
         LR    R0,R5                                                            
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R5,R1                                                            
         SPACE 2                                                                
FORM37   EDIT  (R5),(5,10(R3)),1                                                
         S     R3,AVEDISP                                                       
         B     FORM40                                                           
         SPACE 2                                                                
FORM38   LA    R5,5(R5)                                                         
         EDIT  (R5),(6,DMCB)                                                    
         CLI   TOTDISP,X'FF'                                                    
         BE    FORM38B                                                          
         A     R3,TOTDISP                                                       
         MVC   10(5,R3),DMCB                                                    
         S     R3,TOTDISP                                                       
         SPACE 2                                                                
FORM38B  CLI   AVEDISP,X'FF'                                                    
         BE    FORM40                                                           
         A     R3,AVEDISP                                                       
         LR    R0,R5                                                            
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R5,R1                                                            
         CH    R5,=H'9999'                                                      
         BNH   FORM37                                                           
         EDIT  (R5),(6,DMCB)                                                    
         MVC   10(5,R3),DMCB                                                    
         S     R3,AVEDISP                                                       
         SPACE 2                                                                
FORM40   DS    0H                                                               
         L     R1,ACDOL                                                         
         CLI   CPMDISP,X'FF'                                                    
         BE    FORM46                                                           
         SPACE 2                                                                
FORM42   LTR   R1,R1               CPM                                          
         BZ    FORM46                                                           
         A     R3,CPMDISP                                                       
         LR    RF,R1                                                            
         M     R0,=F'2000'                                                      
         L     R5,8(R2)                                                         
         LTR   R5,R5                                                            
         BZ    FORM44                                                           
         DR    R0,R5                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,2(R3)),2,FLOAT=$                                         
         SPACE 2                                                                
FORM44   LR    R1,RF               CPP                                          
         L     R5,4(R2)                                                         
         LTR   R5,R5                                                            
         BZ    FORM45                                                           
         M     R0,=F'20'                                                        
         DR    R0,R5                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(5,10(R3)),FLOAT=$                                          
         SPACE 2                                                                
FORM45   S     R3,CPMDISP                                                       
         SPACE 2                                                                
FORM46   LA    R2,12(R2)                                                        
         LA    R3,17(R3)                                                        
         BCT   R4,FORM32                                                        
         MVI   P+44,C' '     ****** FUDGE TO FIX BUG ******                     
         EDIT  (4,ACUN),(4,P+40)   **** BRUTE FORCE TO THE MAX                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R3,ATWA                                                          
         USING T31EFFD,R3                                                       
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         BAS   RE,BILDTITL                                                      
         BAS   RE,HOOKEST                                                       
         CLC   FLAVOR(2),=C'VA'                                                 
         BNE   *+10                                                             
         MVC   H6+45(16),=C'(AVERAGE OPTION)'                                   
         MVC   H6+76(13),=C'DAYPART - ALL'                                      
         CLI   SPLDPTH+5,0                                                      
         BE    *+10                                                             
         MVC   H6+86(8),SPLDPTN                                                 
         CLI   DETAIL,C'N'                                                      
         BE    HK2                                                              
         MVC   H5+76(9),=C'NETWORK -'                                           
         MVC   H5+86(4),NBSELNET                                                
         OC    NBSELNET,NBSELNET                                                
         BNZ   HK2                                                              
         MVC   H5+86(4),=C'ALL '   FOR ALL NETWORKS                             
HK2      CLI   SPLPAKH+5,0                                                      
         BE    SHOOK2                                                           
         CLI   NBSELEST,0                                                       
         BE    SHOOK2                                                           
         CLI   NBSELESE,0                                                       
         BNE   SHOOK2                                                           
         MVC   H6+76(34),SPLPAKN                                                
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINES TO HANDLE SUB-LINES                                     
         SPACE 3                                                                
SHOOK2   CLI   FLAVOR,C'P'                                                      
         BNE   SHOOK4                                                           
         BAS   RE,HOOKDEM                                                       
         B     SHOOK10                                                          
         SPACE 2                                                                
SHOOK4   CLI   FLAVOR,C'V'                                                      
         BNE   SHOOK10                                                          
         ZIC   R3,NDNDEMOS                                                      
         CH    R3,=H'3'                                                         
         BL    *+8                                                              
         LA    R3,3                                                             
         LTR   R3,R3               IF ZERO DEMOS, SKIP THIS                     
         BZ    SHOOK10                                                          
         LA    R4,H10+60                                                        
         SR    R2,R2               R2 IS DEMO NUMBER                            
*                                                                               
SHOOK6   NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),DBLOCK,(7,WORK)                    
*                                                                               
         MVC   4(7,R4),WORK                                                     
         LR    R5,R4                                                            
         LA    R6,15                                                            
         SPACE 2                                                                
SHOOK8   CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         BCT   R6,SHOOK8                                                        
         MVC   132(15,R4),=C'VPH  IMPS  GRPS'                                   
         LA    R2,1(R2)                                                         
         LA    R4,17(R4)                                                        
         BCT   R3,SHOOK6                                                        
         SPACE 2                                                                
SHOOK10  CLI   DETAIL,C'P'                                                      
         BNE   HKNOTP                                                           
         MVC   H10(12),=C'PROGRAM NAME'                                         
HKNOTP   CLI   DETAIL,C'N'                                                      
         BNE   HKNOTN                                                           
         MVC   H10+6(7),=C'NETWORK'                                             
         MVC   H11+6(7),=12C'-'                                                 
HKNOTN   CLI   DETAIL,C'E'                                                      
         BNE   HKNOTE                                                           
         MVC   H10+4(8),=C'ESTIMATE'                                            
         MVC   H11+4(8),=12C'-'                                                 
HKNOTE   CLI   DOPT,C'M'                                                        
         BNE   HKOPTW                                                           
         MVC   H10+20(5),=C'MONTH'                                              
         MVC   H11+20(5),=12C'-'                                                
HKOPTW   CLI   DOPT,C'W'                                                        
         BNE   HKOPTQ                                                           
         MVC   H10+20(4),=C'WEEK'                                               
         MVC   H11+20(4),=12C'-'                                                
HKOPTQ   CLI   DOPT,C'Q'                                                        
         BNE   HKFLAVE                                                          
         MVC   H10+19(7),=C'QUARTER'                                            
         MVC   H11+19(7),=12C'-'                                                
HKFLAVE  CLI   FLAVOR,C'E'                                                      
         BNE   XIT                                                              
         IC    R1,FILTER                                                        
         SLL   R1,28                                                            
         SRL   R1,28               0-4                                          
         MH    R1,=H'9'                                                         
         LA    R1,FILTTITL(R1)                                                  
         MVC   H6+49(9),0(R1)                                                   
         LA    R3,H10+53                                                        
         BAS   RE,SOFTHEAD                                                      
         B     XIT                                                              
         SPACE 2                                                                
FILTTITL DC    C'(ORDERED)'                                                     
         DC    C'(CLEARED)'                                                     
         DC    C'UNCLEARED'                                                     
         DC    C' (BILLED)'                                                     
         DC    C' BILLABLE'                                                     
         EJECT                                                                  
*              ROUTINES TO EDIT COLUMNS                                         
         SPACE 3                                                                
         DROP  R6                                                               
         USING ACCUMD,R2                                                        
SOFTCOL  NTR1                                                                   
         ZIC   R1,MENU             (R2=ADDRESS OF ACCUMS)                       
         BCTR  R1,0                (R3=ADDRESS OF FIRST COL)                    
         MH    R1,=H'5'                                                         
         LA    R1,MENUTAB(R1)                                                   
         LA    R0,5                                                             
         SPACE 2                                                                
SFT2     CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,SFT4                                                          
         LA    R3,12(R3)                                                        
         LA    R1,1(R1)                                                         
         BCT   R0,SFT2                                                          
         B     XIT                                                              
         SPACE 2                                                                
SFT4     NTR1                                                                   
         ZAP   MYDUB,=P'0'                                                      
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     BRANCH(RF)                                                       
         SPACE 2                                                                
BRANCH   B     SFT6                1    ASS                                     
         B     SFT8                2    ASS  +2NT +CUT                          
         B     SFT10               3    ACT                                     
         B     SFT12               4    ACT  +INT +CUT                          
         B     SFT10               5    GRSS                                    
         B     SFT16               6    DIFF                                    
         B     SFT18               7    INT                                     
         B     SFT20               8    CUT                                     
         B     SFT22               9    ACT  +INT                               
         B     SFT12               10   ACT  +INT +CUT                          
         B     SFT28               11   EST-PAK-LIN                             
         B     SFT24               12   COMM                                    
         B     SFT26               13   NET                                     
         B     SFT27                                                            
         SPACE 2                                                                
SFT6     L     R1,ACASS            ASS                                          
         BAS   RE,S100                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT8     L     R1,ACASS            ASS  +CUT +INT                               
         B     SFT14                                                            
         SPACE 1                                                                
SFT10    L     R1,ACACT            ACT                                          
         BAS   RE,S100                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT12    L     R1,ACACT            ACT  +CUT +INT                               
         SPACE 1                                                                
SFT14    BAS   RE,S100                                                          
         L     R1,ACCUT                                                         
         BAS   RE,SADD                                                          
         L     R1,ACINT                                                         
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT16    L     R1,ACASS            DIFFERENCE                                   
         S     R1,ACACT                                                         
         BAS   RE,S100                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT18    L     R1,ACINT            INTEGRATION                                  
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT20    L     R1,ACCUT            CUT-IN                                       
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT22    L     R1,ACINT            ACT  +INT                                    
         BAS   RE,SADD                                                          
         B     SFT10                                                            
         SPACE 1                                                                
SFT24    L     R1,ACACT            COMMISSION                                   
         BAS   RE,S85                                                           
         MVC   WORK,MYDUB                                                       
         ZAP   MYDUB,=P'0'                                                      
         BAS   RE,S100                                                          
         SP    MYDUB,WORK(8)                                                    
         B     SFTEND                                                           
         SPACE 1                                                                
SFT26    L     R1,ACACT            NET                                          
         BAS   RE,S85                                                           
         B     SFTEND                                                           
         SPACE 1                                                                
SFT27    L     R1,ACACT                                                         
         BAS   RE,S85                                                           
         L     R1,ACINT                                                         
         M     R0,=F'85'                                                        
         D     R0,=F'100'                                                       
         CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         SPACE 1                                                                
SFTEND   EDIT  (P8,MYDUB),(12,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         B     XIT                                                              
         SPACE 1                                                                
SFT28    B     XIT                                                              
         SPACE 2                                                                
S100     CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         SPACE 2                                                                
SADD     CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         SPACE 2                                                                
S85      CVD   R1,DUB                                                           
         MP    DUB,=P'85'                                                       
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINES TO FILL HEADLINES                                       
         SPACE 3                                                                
SOFTHEAD NTR1                                                                   
         ZIC   R2,MENU             (R3=ADDRESS OF HEADLINE)                     
         BCTR  R2,0                                                             
         MH    R2,=H'5'                                                         
         LA    R2,MENUTAB(R2)                                                   
         LA    R4,5                                                             
         SPACE 2                                                                
SH2      ZIC   R1,0(R2)            GET DATA NUMBER                              
         LTR   R1,R1                                                            
         BZ    SH6                                                              
         BCTR  R1,0                                                             
         MH    R1,=H'12'                                                        
         LA    R1,TITLTAB(R1)      LOCATE TITLE                                 
         MVC   0(11,R3),1(R1)      MOVE IT IN                                   
         LR    R1,R3                                                            
         LA    R0,12                                                            
         SPACE 2                                                                
SH4      CLI   0(R1),C' '          UNDERLINE                                    
         BE    *+8                                                              
         MVI   132(R1),C'-'                                                     
         LA    R1,1(R1)                                                         
         BCT   R0,SH4                                                           
         SPACE 2                                                                
SH6      LA    R2,1(R2)                                                         
         LA    R3,12(R3)                                                        
         BCT   R4,SH2                                                           
         B     XIT                                                              
         SPACE 2                                                                
MENUTAB  DS    0H                  COL  1    2    3    4    5                   
         DC    AL1(3,7,10,0,0)     1    ACT  INT  TOT                           
         DC    AL1(3,7,10,11,0)    2    ACT  INT  TOT  LIST                     
         DC    AL1(1,3,6,7,0)      3    ASS  ACT  DIFF INT                      
         DC    AL1(2,4,6,7,0)      4    ASS+ ACT+ DIFF INT                      
         DC    AL1(5,0,0,0,0)      5    GRSS                                    
         DC    AL1(5,13,0,0,0)     6    GRSS NET                                
         DC    AL1(5,7,09,0,0)     7    GRSS INT  TOT                           
         DC    AL1(5,7,09,14,0)    8    GRSS INT  TOT  NET                      
         DC    AL1(5,12,13,0,0)    9    GRSS COMM NET                           
         DC    AL1(5,7,9,14,11)    10   GRSS INT  TOT  NET  LIST                
         DC    AL1(7,0,0,0,0)      11   INT                                     
         DC    5X'00'              12   SPARE                                   
         SPACE 2                                                                
TITLTAB  DS    0H                                                               
         DC    CL12'    ASSIGNED'  1                                            
         DC    CL12'    ASSIGNED'  2 (+INT +CUT)                                
         DC    CL12'      ACTUAL'  3                                            
         DC    CL12'      ACTUAL'  4 (+INT +CUT)                                
         DC    CL12'       GROSS'  5                                            
         DC    CL12'  DIFFERENCE'  6                                            
         DC    CL12' INTEGRATION'  7                                            
         DC    CL12'      CUT-IN'  8                                            
         DC    CL12'       TOTAL'  9  (ACT + INT)                               
         DC    CL12'       TOTAL'  10 (ACT + INT + CUT)                         
         DC    CL12' '                                                          
         DC    CL12'  COMMISSION'  12                                           
         DC    CL12'         NET'  13                                           
         DC    CL12'     NET TOT'  14                                           
         EJECT                                                                  
*              ROUTINE TO BUILD TITLE                                           
         SPACE 3                                                                
BILDTITL NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(8),=C'POST-BUY'                                             
         CLI   FLAVOR,C'P'                                                      
         BE    BT2                                                              
         MVC   WORK(8),=C'ESTIMATE'                                             
         CLI   FLAVOR,C'E'                                                      
         BE    BT2                                                              
         MVC   WORK(10),=C'EVALUATION'                                          
         SPACE 2                                                                
BT2      MVC   WORK+12(6),=C'WEEKLY'                                            
         CLI   DOPT,C'W'                                                        
         BE    BT4                                                              
         MVC   WORK+12(9),=C'QUARTERLY'                                         
         CLI   DOPT,C'Q'                                                        
         BE    BT4                                                              
         MVC   WORK+12(10),=CL10'MONTHLY'                                       
         SPACE 2                                                                
BT4      MVC   WORK+24(7),=C'SUMMARY'                                           
         GOTO1 SQUASHER,DMCB,WORK,36                                            
         MVC   H1+40(30),WORK                                                   
         GOTO1 UNDERLIN,DMCB,(30,H1+40),H2+40                                   
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES - DEMOS                                        
         SPACE 3                                                                
HOOKDEM  NTR1                                                                   
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(5,WORK)                       
         MVI   H11+56,C' '                                                      
         MVC   H11+57(5),WORK                                                   
         MVC   H12+57(5),WORK+5                                                 
         CLI   NDNDEMOS,1                                                       
         BE    HOOKDEM2                                                         
         NETGO NVDEMCON,DMCB,(1,NDDEMBLK),DBLOCK,(5,WORK)                       
         MVC   H11+64(5),WORK                                                   
         MVC   H12+64(5),WORK+5                                                 
*                                                                               
HOOKDEM2 GOTO1 UNDERLIN,DMCB,(46,H11+33),H10+33                                 
         MVC   H10+74(36),H10+33                                                
         MVC   H11+74(36),H11+33                                                
         MVC   H12+74(36),H12+33                                                
         CLI   NDNDEMOS,1                                                       
         BE    HOOKDEM4                                                         
         MVC   H10+46(9),=C'ESTIMATED'                                          
         MVC   H10+89(6),=C'ACTUAL'                                             
         B     XIT                                                              
         SPACE 2                                                                
HOOKDEM4 MVC   H10+43(9),=C'ESTIMATED'                                          
         MVC   H10+86(6),=C'ACTUAL'                                             
         B     XIT                                                              
         SPACE 3                                                                
*              HEADLINE ROUTINES - ESTIMATE                                     
         SPACE 3                                                                
HOOKEST  NTR1                                                                   
         CLI   NBSELESE,0                                                       
         BE    XIT                                                              
         MVC   WORK(29),=C'ESTIMATES 101 TO 102 COMBINED'                       
         EDIT  (1,NBSELEST),(3,WORK+10),WRK=DMCB                                
         EDIT  (1,NBSELESE),(3,WORK+17),WRK=DMCB                                
         GOTO1 SQUASHER,DMCB,WORK,29                                            
         MVC   H6(38),SPACES                                                    
         MVC   H6(29),WORK                                                      
         B     XIT                                                              
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
NETTBL   DS    CL4000                                                           
         EJECT                                                                  
*              DSECTS FOR MONTHLY SUMMARIES                                     
         SPACE 3                                                                
POSTD    DSECT                                                                  
*              NETDEMOD HERE                                                    
*              AND DEDBLOCK                                                     
         PRINT OFF                                                              
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
*                                  ARGUMENTS FROM EDIT                          
FLAVOR   DS    CL1                                                              
DOPT     DS    CL1                                                              
DETAIL   DS    CL1                                                              
FILTER   DS    CL1                                                              
         SPACE 1                                                                
RELO     DS    F                                                                
*                                  LOCAL WORKING STORAGE                        
ACTDOLS  DS    F                   ACTUAL COST IN DOLLARS                       
ASSDOLS  DS    F                   ASSIGNED COST IN DOLLARS                     
*                                                                               
MYDUB    DS    D                                                                
SAVEPROG DS    CL16                                                             
SAVENET  DS    CL4                                                              
SAVEPAK  DS    CL1                                                              
SAVEEST  DS    CL1                                                              
*                                                                               
TOTFLAG  DS    CL1                 SET IF CPM LINE TO BE PRINTED                
*                                   BYPASSES OLD LOGIC                          
PERTYPE  DS    CL3                 1ST BYTE IS PERIOD TYPE                      
MAXMONTS EQU   16                  MAX NUM OF DATE SETS IN MONTH LIST           
NUMMONS  DS    F                   CURR NUM DATE SETS IN LIST                   
MONLIST  DS    CL(4*MAXMONTS)                                                   
*                                                                               
TOTDISP  DS    F                                                                
CPMDISP  DS    F                                                                
AVEDISP  DS    F                                                                
IMPS     DS    F                                                                
EPL      DS    CL16                                                             
VPHSW    DS    CL1                                                              
MENU     DS    CL1                                                              
SAVLINE  DS    CL1                                                              
FRSTUNIT DS    CL1                                                              
*                                                                               
SPOTAC   DS    0C                                                               
WORKAC   DS    CL64                                                             
PROGAC   DS    17CL64                                                           
NETAC    DS    17CL64                                                           
ESTAC    DS    17CL64                                                           
NDOLTBL  DS    CL160                                                            
REPTOT   DS    CL4                                                              
         SPACE 3                                                                
*              DSECTS TO COVER ACCUMULATORS                                     
         SPACE 3                                                                
ACCUMD   DSECT                                                                  
ACUN     DS    F                                                                
ACHUT    DS    F                                                                
ACSHR    DS    F                                                                
ACDOL    DS    F                                                                
ACHOMVPH DS    F                                                                
ACHOMRTG DS    F                                                                
ACHOMIMP DS    F                                                                
ACDEMVPH DS    F                                                                
ACDEMRTG DS    F                                                                
ACDEMIMP DS    F                                                                
         SPACE 2                                                                
         ORG   ACUN                                                             
*                       ESTIMATES                                               
ACUNE    DS    F                   UNITS                                        
ACASS    DS    F                   ASSIGNED COST                                
ACACT    DS    F                   ACTUAL COST                                  
ACINT    DS    F                   INTEGRATION                                  
ACCUT    DS    F                   CUT IN                                       
         SPACE 2                                                                
         ORG   ACUN                                                             
*                       POST-BUY                                                
ACCOST   DS    F                   ESTIMATED COST                               
ACUNITS  DS    F                             UNITS                              
ACGRPS   DS    F                             HOME GRPS                          
ACHOMES  DS    F                             HOME IMPS                          
ACDEMA   DS    F                             DEMO A                             
ACDEMB   DS    F                             DEMO B                             
ACACTUAL DS    6F                  ACTUAL    AS ABOVE                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEBD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED6BT  05/01/02'                                      
         END                                                                    

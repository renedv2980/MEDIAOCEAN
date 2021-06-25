*          DATA SET NEMED85S   AT LEVEL 028 AS OF 05/01/02                      
*PHASE T31E85A                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T31E85 - DRIVER TEST PHASE'                                     
T31E85   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TEST**,RR=R2                                                 
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BE    PRIO                                                             
         CLI   MODE,VALKEY                                                      
         BNE   XIT                                                              
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         SPACE 1                                                                
ED2      LA    R2,PRGNETH                                                       
         LA    R3,NETLIST                                                       
         LA    R4,4                MAX 4                                        
         LA    R5,1                COUNT IN 5                                   
         SPACE 1                                                                
ED4      NETGO NVNET,DMCB,5(R3)    VALIDATE NETWORK. PUT MKT AT 5+R3            
         CLI   5(R2),0             CK FOR LAST                                  
         BE    ED6                                                              
         MVI   FTERMFLG,1          ONLY FIRST IS REQUIRED                       
         STC   R5,NNETS                                                         
         MVC   0(3,R3),FLD         BUILD STA LIST                               
         MVI   3(R3),X'40'                                                      
         MVI   4(R3),C'N'          FOR NETWORK                                  
         LA    R3,7(R3)                                                         
         LA    R5,1(R5)                                                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,ED4                                                           
         SPACE 1                                                                
ED6      MVI   FTERMFLG,0          PERIOD (REQUIRED)                            
         LA    R2,PRGSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
         GOTO1 DATCON,DMCB,(0,USERQSTR),(2,CMPSTART)                            
         LA    R2,PRGENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
         GOTO1 DATCON,DMCB,(0,USERQEND),(2,CMPEND)                              
         SPACE 1                                                                
         MVI   FTERMFLG,1          DAY/TIME(OPTIONAL)                           
         LA    R2,PRGDAYH                                                       
         NETGO NVDAY,DMCB,DAYFILT                                               
         LA    R2,PRGTIMEH                                                      
         MVC   TIMEFILT(2),=H'600'                                              
         MVC   TIMEFILT+2(2),=H'2600'                                           
         CLI   5(R2),0                                                          
         BE    ED8                 DEFAULT IS ALL                               
         ZIC   R3,5(R2)                                                         
         GOTO1 CALLOV,DMCB,0,X'D9000A0E'                                        
         L     RF,DMCB             PICK UP A(TIMVAL)                            
         GOTO1 (RF),DMCB,((R3),8(R2)),TIMEFILT                                  
         CLI   DMCB,X'FF'          ANY ERRORS?                                  
         BNE   ED8                                                              
         MVI   ERROR,INVTIME                                                    
         B     EDERR                                                            
         SPACE 1                                                                
ED8      LA    R2,PRGFILTH                                                      
         XC    FILTER,FILTER                                                    
         CLI   5(R2),0                                                          
         BE    ED10                                                             
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ED10                                                             
         MVC   FILTER(0),8(R2)                                                  
         SPACE 1                                                                
ED10     LA    R2,PRGPTYPH                                                      
         XC    PTYPFILT,PTYPFILT                                                
         CLI   5(R2),0                                                          
         BE    ED12                                                             
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     ED12                                                             
         MVC   PTYPFILT(0),8(R2)                                                
         SPACE 1                                                                
ED12     LA    R2,PRGRANKH                                                      
         MVC   RANK,=C'A1'         DEFAULT IS RANK ON FIRST ACTUAL              
         CLI   5(R2),0                                                          
         BE    ED20                                                             
         MVC   RANK,8(R2)                                                       
         CLI   RANK,C'A'           FIRST S/B A(CTUAL) E(ST) OR I(NDEX)          
         BE    ED14                                                             
         CLI   RANK,C'E'                                                        
         BE    ED14                                                             
         CLI   RANK,C'I'                                                        
         BNE   EDINV                                                            
         SPACE 1                                                                
ED14     CLI   RANK+1,C'S'         SECOND S/B S OR 1-5                          
         BE    ED20                                                             
         CLI   RANK+1,C'1'                                                      
         BL    EDINV                                                            
         CLI   RANK+1,C'5'                                                      
         BH    EDINV                                                            
         SPACE 1                                                                
ED20     LA    R2,PRGDEMH          DEMOS(COMPULSORY)                            
         MVI   FTERMFLG,1                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         SPACE 1                                                                
         LA    R2,PRGOPTH                                                       
         BAS   RE,VALIOPT                                                       
         B     EDEX                                                             
         SPACE 1                                                                
EDINV    MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         SPACE 1                                                                
EDEX     LA    R2,PRGNETH                                                       
         XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VALIOPT  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    OPTERR                                                           
         SPACE 1                                                                
VALIOPT2 DS    0H                                                               
         SPACE 1                                                                
VALIOPT6 B     OPTERR                                                           
         SPACE 1                                                                
VALIOPTX LA    R3,32(R3)                                                        
         BCT   R4,VALIOPT2                                                      
         B     XIT                                                              
         SPACE 1                                                                
OPTERR   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
*              SET UP FOR REPORTING                                             
         SPACE 3                                                                
PRIO     L     R6,=V(DUMMY)                                                     
         A     R6,RELO                                                          
         USING GLOBALD,R6                                                       
         LR    RE,R6               CLEAR GLOBALS                                
         L     RF,=F'20000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'20000'    SET UP COMPULSORIES                          
         GOTO1 =V(LOADER),DMCB,=CL8'DRIVER',0                                   
         MVC   DRIVER,DMCB+4                                                    
         LA    R2,SIMPPROG                                                      
         ST    R2,GLAPROG                                                       
         LA    R2,SYSDRIV                                                       
******   ST    R2,GLASYSDR                                                      
******   LA    R2,PROGHOOK                                                      
         ST    R2,GLAHOOK                                                       
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,1                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         MVI   GLLHEADL,9                                                       
         SPACE 1                                                                
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
         EJECT                                                                  
*              INPUT - PROGRAM I/O                                              
         SPACE 3                                                                
         LA    R2,NETLIST                                                       
         ZIC   R3,NNETS            HANDLE N NETWORKS                            
         MVI   USEIO,C'Y'          READ INTO I/O AREA                           
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOTFILE                      
         SPACE 1                                                                
PROGRAMS MVC   ACTNET(7),0(R2)                                                  
         BAS   RE,PR1                                                           
         LA    R2,7(R2)                                                         
         BCT   R3,PROGRAMS                                                      
         MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
PR1      NTR1                                                                   
         MVI   DMFILE,C'S'                                                      
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         MVC   NPGKNET,ACTNET+5                                                 
         MVC   FILENAME,=CL7'SPTDIR '                                           
         GOTO1 HIGH                                                             
         B     PR4                                                              
         SPACE 1                                                                
PR2      LA    R4,KEY                                                           
         MVC   FILENAME,=CL7'SPTDIR '                                           
         GOTO1 SEQ                                                              
         SPACE 1                                                                
PR4      CLC   KEY(5),KEYSAVE      CHECK C/B                                    
         BNE   XIT                                                              
         CLC   NPGKEND,CMPSTART    IGNORE THOSE BEFORE START                    
         BL    PR2                                                              
         CLC   NPGKEND,CMPEND      OR AFTER END                                 
         BH    PR2                                                              
         CLC   NPGKPROG,LASTPROG                                                
         BNE   PR6                                                              
         CLC   LASTPROG+6(2),CMPEND     ALREADY HAVE A PREVIOUS ENTRY           
         BH    PR2             FOR PROGRAM COVERING REQUESTED END DATE          
         SPACE 1                                                                
PR6      MVC   FILENAME,=CL7'SPTFILE'                                           
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGELEM,R5                                                       
         CLI   DAYFILT,X'FF'       POSSIBLE DAY FILTER                          
         BE    PR8                                                              
         CLC   DAYFILT,NPGRDAY                                                  
         BNE   PR2                                                              
         SPACE 1                                                                
PR8      CLC   NPGTIME(2),TIMEFILT  TIME FILTERS                                
         BL    PR2                                                              
         OC    NPGTIME+2(2),NPGTIME+2                                           
         BZ    PR10                                                             
         OC    TIMEFILT+2(2),TIMEFILT+2                                         
         BZ    PR10                                                             
         CLC   NPGTIME+2(2),TIMEFILT+2                                          
         BH    PR2                                                              
         SPACE 1                                                                
PR10     ZIC   R3,PRGFILTH+5                                                    
         LTR   R3,R3               ANY FILTERS                                  
         BZ    PR14                                                             
         LA    R2,PRGFILT                                                       
         LA    R4,NPGFILT                                                       
         SPACE 1                                                                
PR12     CLI   0(R2),C'A'          YES - SO FILTER                              
         BL    *+14                                                             
         CLC   0(1,R2),0(R4)                                                    
         BNZ   PR2                                                              
         LA    R2,1(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,PR12                                                          
         SPACE  1                                                               
PR14     MVC   LASTPROG,NPGKPROG   SAVE PROGRAM AND END DATE                    
         EJECT                                                                  
*              FILL IN PROGRAM DETAILS                                          
         SPACE 3                                                                
         LA    R2,PROGITEM                                                      
         USING POOLD,R2                                                         
         XC    POOLITEM,POOLITEM   SET UP NEW PROGRAM                           
         MVC   POOLNET,ACTNET      FILL A POOL ENTRY                            
         MVC   POOLPROG,NPGNAME                                                 
         MVC   POOLPCOD,NPGKPROG                                                
         MVC   POOLDAY,NPGRDAY                                                  
         MVC   POOLTIME,NPGTIME                                                 
         MVC   POOLNTI,NPGPPNO                                                  
         MVC   POOLESHR,NPGSHARE   ESTIMATED SHARE/RATING                       
         MVC   POOLSTAT,NPGSTAT                                                 
         SPACE 1                                                                
         XC    PIO,PIO             BUILD PHONY EVN RECORD                       
         MVI   PUEL,X'31'                                                       
         MVI   PUEL+1,99                                                        
         MVC   PVEL(3),=X'332501'                                               
         MVC   PVEL+3(34),NPGVPHS                                               
         MVI   PIOEOR,0                                                         
         XC    PREL,PREL                                                        
         MVC   PREL(3),=X'350902'  GENERATE RATINGS ELEMENT                     
         MVC   PREL+3(2),NPGSHARE                                               
         MVC   PREL+7(2),NPGSHARE                                               
         SPACE 1                                                                
EVN2     MVI   ELCODE,X'5D'        PICK UP A BOOK ELEMENT                       
         BAS   RE,NEXTEL                                                        
         MVC   PBEL,0(R5)                                                       
         SPACE 1                                                                
         MVC   DBFILE,=C'EVN'                                                   
         LA    R1,PIO                                                           
         ST    R1,DBAREC                                                        
         LA    R1,PUEL                                                          
         ST    R1,DBAQUART                                                      
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         XC    DEMS,DEMS                                                        
*******  GOTO1 NBDEMOUT,DMCB,(C'L',NDDEMOS),DBLOCK,DEMS                         
         MVC   POOLVPHS(4),=F'250'                                              
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     PR2                                                              
         EJECT                                                                  
*              ROUTINES TO SIMULATE SYSTEM DRIVER                               
         SPACE 3                                                                
SYSDRIV  NTR1                                                                   
         CLI   GLHOOK,GLRESOLV                                                  
         BE    SYSINIT                                                          
         CLI   GLHOOK,GLROUT                                                    
         BNE   XIT                                                              
         CLI   GLMODE,GLINPUT                                                   
         BE    SYSINP                                                           
         CLI   GLMODE,GLOUTPUT                                                  
         BE    SYSOUTP                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
SYSINIT  LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
         SPACE 1                                                                
SYSINIT2 CLC   0(8,R1),0(R2)                                                    
         BE    SYSINIT4                                                         
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSINIT2                                                         
         SPACE 1                                                                
SYSINIT4 MVC   GLAROUT,8(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    C'NET     ',A(INNET)                                             
         DC    C'DAY     ',A(INDAY)                                             
         DC    C'DAYOUT  ',A(OUTDAY)                                            
         DC    C'TIME    ',A(INTIM)                                             
         DC    C'TIMOUT  ',A(OUTTIM)                                            
         DC    C'PROGRAM ',A(INNAM)                                             
         DC    C'DEMO    ',A(INDEM)                                             
         DC    X'FF'                                                            
         SPACE 1                                                                
SYSINP   L     RF,GLAROUT                                                       
         LA    R2,PROGITEM                                                      
         USING POOLITEM,R2                                                      
         L     R3,GLAIFLD                                                       
         BR    RF                                                               
         SPACE 1                                                                
INNET    MVC   0(4,R3),POOLNET                                                  
         B     XIT                                                              
         SPACE 1                                                                
INDAY    MVC   0(1,R3),POOLDAY                                                  
         B     XIT                                                              
         SPACE 1                                                                
INTIM    MVC   0(4,R3),POOLTIME                                                 
         B     XIT                                                              
         SPACE 1                                                                
INNAM    MVC   0(16,R3),POOLPROG                                                
         B     XIT                                                              
         SPACE 1                                                                
INDEM    MVC   0(2,R3),POOLESHR                                                 
         B     XIT                                                              
         SPACE 1                                                                
SYSOUTP  L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BR    RF                                                               
OUTDAY   ZIC   R1,0(R2)            DAY                                          
         MH    R1,=H'3'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   0(3,R3),0(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
OUTTIM   GOTO1 UNTIME,DMCB,(R2),(R3)                                            
         B     XIT                                                              
         EJECT                                                                  
*              APPLICATION HOOK FROM DRIVER                                     
         SPACE 3                                                                
PROGHOOK NTR1                                                                   
         B     XIT                                                              
         EJECT                                                                  
*              HARD CODED SIMPLE PROGRAM                                        
         SPACE 3                                                                
SIMPPROG DS    0D                                                               
         DC    X'1002'             RECORD START                                 
         SPACE 1                                                                
         DC    X'2002'             NETWORK                                      
         DC    X'2204C300'                                                      
         DC    X'230304'                                                        
         DC    X'240A',CL8'NET'                                                 
         DC    X'3002'                                                          
         DC    X'3204C300'                                                      
         DC    X'330304'                                                        
         DC    X'400301'                                                        
         DC    X'870600',CL3'NET'                                               
         DC    X'4403',C'L'                                                     
         DC    X'46040201'                                                      
         DC    X'4802'                                                          
         DC    X'46040100'                                                      
         DC    X'870600'                                                        
         DC    C'TOT'                                                           
         SPACE 1                                                                
         DC    X'2002'             DAY                                          
         DC    X'2204D700'                                                      
         DC    X'230301'                                                        
         DC    X'240A',CL8'DAY'                                                 
         DC    X'3002'                                                          
         DC    X'3204C300'                                                      
         DC    X'330303'                                                        
         DC    X'340A',CL8'DAYOUT'                                              
         DC    X'400301'                                                        
         DC    X'870600',CL3'DAY'                                               
         SPACE 1                                                                
         DC    X'2002'             TIME                                         
         DC    X'2204D700'                                                      
         DC    X'230304'                                                        
         DC    X'240A',CL8'TIME'                                                
         DC    X'3002'                                                          
         DC    X'3204C300'                                                      
         DC    X'33030B'                                                        
         DC    X'340A',CL8'TIMOUT'                                              
         DC    X'400301'                                                        
         DC    X'870700',CL4'TIME'                                              
         SPACE 1                                                                
         DC    X'1202'             END OF KEY                                   
         SPACE 1                                                                
         DC    X'2002'             PROGRAM NAME                                 
         DC    X'2204C300'                                                      
         DC    X'230310'                                                        
         DC    X'240A',CL8'PROGRAM'                                             
         DC    X'3002'                                                          
         DC    X'3204C300'                                                      
         DC    X'330310'                                                        
         DC    X'400301'                                                        
         DC    X'870F00',CL12'PROGRAM NAME'                                     
         SPACE 1                                                                
         DC    X'2002'             SHARE                                        
         DC    X'2204',C'B+'                                                    
         DC    X'230302'                                                        
         DC    X'240A',CL8'DEMO'                                                
         DC    X'250300'                                                        
         DC    X'3002'                                                          
         DC    X'3204D500'                                                      
         DC    X'330305'                                                        
         DC    X'36040101'                                                      
         DC    X'400301'                                                        
         DC    X'870800',CL5'SHARE'                                             
         SPACE 1                                                                
         DC    X'00'               END OF PROGRAM                               
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,49,C'DRIVER PROGRAM LISTING'                                  
         SSPEC H2,49,C'----------------------'                                  
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,PERIOD                                                      
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,99,PAGE                                                       
         DC    X'00'                                                            
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
         SPACE 3                                                                
HOOK     NTR1                                                                   
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              STORAGE  LTORG ETC                                               
         SPACE 3                                                                
RELO     DS    A                                                                
APOOL    DS    A                                                                
DRIVER   DS    A                                                                
         SPACE 1                                                                
WPOOL    DC    H'68'                                                            
PROGITEM DC    68X'00'                                                          
POOLMAX  DC    F'2000'                                                          
POOLNOW  DC    F'0'                                                             
DAYLIST  DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
SDLIST   DC    X'7C402010080402017F'                                            
EAISW    DC    X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
*              NETINCLS AND MODULE W/S                                          
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
MYD      DSECT                                                                  
*                                  DEDBLOCK & NETDEMOD                          
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NETDEMOD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
NNETS    DS    CL1                 NUMBER OF NETS                               
NETLIST  DS    4CL7                STATION LIST                                 
ACTNET   DS    CL7                                                              
LASTPROG DS    CL6                                                              
DAYFILT  DS    CL1                 FILTERS                                      
TIMEFILT DS    CL4                                                              
FILTER   DS    CL4                                                              
PTYPFILT DS    CL4                                                              
RANK     DS    CL2                                                              
PDISP    DS    F                                                                
LASTR5   DS    F                                                                
MYCOLS   DS    CL132                                                            
MYHEDA   DS    CL132                                                            
MYHEDB   DS    CL132                                                            
CMPSTART DS    CL2                                                              
CMPEND   DS    CL2                                                              
PIO      DS    0CL150                                                           
         DS    CL22                                                             
PUEL     DS    CL99                                                             
PVEL     DS    CL37                                                             
PREL     DS    CL9                                                              
PBEL     DS    CL7                                                              
PIOEOR   DS    CL1                                                              
DEMS     DS    CL24                                                             
DEFINE   DS    A                                                                
DEMOCON  DS    A                                                                
NETWEEK  DS    A                                                                
STWEEK   DS    CL2                                                              
NDWEEK   DS    CL2                                                              
THISWEEK DS    CL2                                                              
RALPH    DS    CL30                                                             
         SPACE 1                                                                
MTHBLOCK DS    0CL17               DEMOMATH BLOCK                               
MTHDB    DS    A                            A(DBLOCK)                           
MTHFCTR  DS    F                            FACTOR                              
MTHIFIL  DS    CL3                          INPUT FILE                          
MTHOFIL  DS    CL3                          OUTPUT FILE                         
MTHOSRC  DS    CL3                          OUTPUT SOURCE                       
         SPACE 1                                                                
DBIO2    DS    1000C                                                            
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD2D                                                       
         SPACE 2                                                                
POOLD    DSECT                                                                  
POOLITEM DS    0CL68               DSECT FOR POOL ITEMS                         
POOLRANK DS    CL4                                                              
POOLPROG DS    CL16                                                             
POOLPCOD DS    CL6                                                              
POOLNET  DS    CL4                                                              
POOLDAY  DS    CL1                                                              
POOLTIME DS    CL4                                                              
POOLNTI  DS    CL2                                                              
POOLPTYP DS    CL2                                                              
POOLWEEK DS    CL1                                                              
POOLSTAT DS    CL1                                                              
         DS    CL3                                                              
         SPACE 1                                                                
POOLESHR DS    CL2                 ESTIMATED SHARE                              
POOLASHR DS    CL2                 ACTUAL SHARE                                 
POOLVPHS DS    0CL20               VPHS (UP TO 5 DEMOS.)                        
POOLEVPH DS    CL2                       ESTIMATED                              
POOLAVPH DS    CL2                       ACTUAL                                 
         DS    CL16                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028NEMED85S  05/01/02'                                      
         END                                                                    

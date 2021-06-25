*          DATA SET NEMED82    AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET NEMED82    AT LEVEL 058 AS OF 05/20/85                      
*PHASE T31E82A,+0                                                               
         TITLE 'T31E82 - PROGRAM POSTS'                                         
T31E82   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PRPO**,R6,RR=R2                                              
         USING T31E82+4096,R6                                                   
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
VALIOPT2 CLC   12(5,R3),=C'INDEX'                                               
         BNE   VALIOPT4                                                         
***      MVC   INDEXOPT,22(R3)                                                  
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT4 CLC   12(6,R3),=C'ACTUAL'                                              
         BNE   VALIOPT6                                                         
***      MVC   ACTOPT,22(R3)                                                    
         B     VALIOPTX                                                         
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
***PRIO     L     R1,=V(DUMMY)                                                  
***         A     R1,RELO                                                       
PRIO     L     R1,VADUMMY                                                       
         ST    R1,APOOL                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         SPACE 1                                                                
         ZIC   R1,NDNDEMOS         FIGURE OUT DISPLACEMENT                      
         LA    R2,5                                                             
         SR    R2,R1                                                            
         MH    R2,=H'6'                                                         
         ST    R2,PDISP                                                         
         XC    POOLNOW,POOLNOW                                                  
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A26'                                        
         MVC   DEFINE,DMCB         PICK UP A(DEFINE)                            
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'                                        
         MVC   DEMOCON,DMCB        PICK UP A(DEMOCON)                           
         GOTO1 CALLOV,DMCB,0,X'D9000A17'                                        
         MVC   NETWEEK,DMCB        PICK UP A(NETWEEK)                           
         SPACE 1                                                                
         LA    R1,DBLOCK           INITIALIZE DEMOMATH BLOCK                    
         ST    R1,MTHDB                                                         
         MVC   MTHIFIL,=C'NTI'                                                  
         MVC   MTHOFIL,=C'NTI'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
         SPACE 1                                                                
         MVC   MYCOLS,SPACES       INITIALIZE HEADINGS & BOXES                  
         MVC   MYHEDA,SPACES                                                    
         MVC   MYHEDB,SPACES                                                    
         LA    R2,MYCOLS                                                        
         LA    R3,MYHEDA                                                        
         A     R2,PDISP                                                         
         A     R3,PDISP                                                         
         MVI   00(R2),C'L'                                                      
         MVI   05(R2),C'C'                                                      
         MVI   22(R2),C'C'                                                      
         MVI   27(R2),C'C'                                                      
         MVI   31(R2),C'C'                                                      
         MVI   37(R2),C'C'                                                      
         MVI   44(R2),C'C'                                                      
         MVI   49(R2),C'C'                                                      
         MVI   54(R2),C'C'                                                      
         MVI   58(R2),C'C'                                                      
         LA    R2,70(R2)                                                        
         MVC   0(36,R3),=C' RANK   PROGRAM NAME   NET  DAY TIME'                
         MVC   132(5,R3),=C'  NO.'                                              
         MVC   039(4,R3),=C'PROG'                                               
         MVC   171(4,R3),=C'CODE'                                               
         MVC   045(25,R3),=C'NTI  PROG WKS  SHARE/RTG*'                         
         MVC   177(25,R3),=C'CODE TYPE RUN EST ACT INX'                         
         LA    R3,71(R3)                                                        
         LA    R4,NDDEMOS                                                       
         ZIC   R0,NDNDEMOS                                                      
         SPACE 1                                                                
PRIN2    MVI   0(R2),C'C'                                                       
         MVC   0(4,R3),=C'VPH/'                                                 
         GOTO1 DEMOCON,DMCB,(R4),(7,WORK),DBLOCK                                
         MVC   4(7,R3),WORK                                                     
         GOTO1 CENTER,DMCB,(R3),11                                              
         MVC   132(11,R3),=C'EST ACT INX'                                       
         LA    R2,12(R2)                                                        
         LA    R3,12(R3)                                                        
         MVI   1(R4),C'V'          FORCE IN VPH                                 
         LA    R4,3(R4)                                                         
         BCT   R0,PRIN2                                                         
         MVI   0(R2),C'R'                                                       
         SPACE 1                                                                
         MVC   RALPH,SPACES        DESCRIBE RANKING BASE                        
         MVC   RALPH(9),=C'RANKED BY'                                           
         MVC   RALPH+10(5),=C'SHARE'                                            
         CLI   RANK+1,C'S'                                                      
         BE    PRIN4                                                            
         ZIC   R2,RANK+1                                                        
         SLL   R2,28                                                            
         SRL   R2,28                                                            
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R2,NDDEMOS(R2)                                                   
         GOTO1 DEMOCON,DMCB,(R2),(7,WORK),DBLOCK                                
         MVC   RALPH+10(7),WORK                                                 
         SPACE 1                                                                
PRIN4    MVC   RALPH+20(5),=C'INDEX'                                            
         CLI   RANK,C'I'                                                        
         BE    PRIN6                                                            
         MVC   RALPH+20(6),=C'ACTUAL'                                           
         CLI   RANK,C'A'                                                        
         BE    PRIN6                                                            
         MVC   RALPH+20(9),=C'ESTIMATED'                                        
         SPACE 1                                                                
PRIN6    GOTO1 SQUASHER,DMCB,RALPH,30                                           
         SPACE 1                                                                
         GOTO1 NETWEEK,DMCB,USERQSTR,NBGETDAY,NBADDAY                           
         MVC   STWEEK(1),DMCB+4                                                 
         MVC   STWEEK+1(1),DMCB+0                                               
         GOTO1 NETWEEK,DMCB,USERQEND,NBGETDAY,NBADDAY                           
         MVC   NDWEEK(1),DMCB+4                                                 
         MVC   NDWEEK+1(1),DMCB+0                                               
         EJECT                                                                  
*              PHASE 1 - PROGRAM I/O                                            
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
         OC    POOLNOW,POOLNOW                                                  
         BZ    XIT                 NO VALID PROGRAMS                            
         BAS   RE,FILLPOOL         FILL POOL WITH NUMBERS                       
         BAS   RE,RANKPOOL         RANK THE POOL                                
         BAS   RE,REPPOOL          REPORT ON THE POOL                           
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
PR2      MVC   FILENAME,=CL7'SPTDIR '                                           
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
*              ADD PROGRAM TO POOL                                              
         SPACE 3                                                                
         L     R2,POOLNOW                                                       
         LA    R2,1(R2)                                                         
         ST    R2,POOLNOW                                                       
         C     R2,POOLMAX          TEST V MAX                                   
         BL    *+6                                                              
         DC    H'0'                NO MORE ROOM                                 
         BCTR  R2,0                                                             
         MH    R2,WPOOL                                                         
         A     R2,APOOL                                                         
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
         GOTO1 NBDEMOUT,DMCB,(C'L',NDDEMOS),DBLOCK,DEMS                         
         MVC   POOLVPHS,DEMS+2                                                  
         B     PR2                                                              
         EJECT                                                                  
*              ROUTINES TO FILL POOL WITH DEMOS                                 
         SPACE 3                                                                
FILLPOOL NTR1                                                                   
         MVC   THISWEEK,STWEEK                                                  
         SPACE 1                                                                
FILL2    BAS   RE,FILL4            NEED TO GO FOR EACH ACTIVE WEEK              
         CLC   THISWEEK,NDWEEK                                                  
         BNE   FILL3                                                            
         BAS   RE,AVEPOOL                                                       
         B     XIT                                                              
         SPACE 1                                                                
FILL3    AI    THISWEEK+1,1        BUMP TO NEXT WEEK                            
         CLI   THISWEEK+1,49                                                    
         BNE   FILL2                                                            
         MVI   THISWEEK+1,1        OR WEEK 1 OF NEXT YEAR                       
         AI    THISWEEK,1                                                       
         B     FILL2                                                            
         SPACE 1                                                                
FILL4    NTR1                                                                   
         L     R2,APOOL                                                         
         L     R0,POOLNOW                                                       
         SPACE 1                                                                
FILL6    BAS   RE,FILL10           FOR EACH WEEK BROWSE ROUND POOL              
         AH    R2,WPOOL                                                         
         BCT   R0,FILL6                                                         
         B     XIT                                                              
         SPACE 1                                                                
FILL10   NTR1                                                                   
         XCEF  DBIO2,1000          INITIALIZE IO2 FOR DEMOMATH                  
         MVI   DBIO2,C'P'                                                       
         MVC   DBIO2+20(2),=H'24'                                               
         SPACE 1                                                                
         MVC   DBFILE,=C'NTI'      SET UP FOR DEMAND                            
         MVC   DBAREC,AIO                                                       
         XC    DBAQUART,DBAQUART                                                
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,THISWEEK                                                 
         MVI   DBSELMED,C'N'                                                    
         MVC   DBSELSTA,POOLNET                                                 
         MVI   DBSELSTA+4,C'T'                                                  
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM,DBSELTIM                                                
         MVC   DBSELPRG,POOLNTI                                                 
         MVI   DBFUNCT,DBGETNTI                                                 
         OC    POOLNTI,POOLNTI                                                  
         BNZ   FILL12                                                           
         SPACE 1                                                                
         ZIC   R1,POOLDAY          NO NTI SO READ FOR DAY/TIME                  
         LA    R1,SDLIST(R1)                                                    
         MVC   DBSELDAY,0(R1)                                                   
         MVC   DBSELTIM,POOLTIME                                                
         MVI   DBFUNCT,DBGETDEM                                                 
         SPACE 1                                                                
FILL12   GOTO1 NBDEMAND,DMCB,DBLOCK,DEMHOOK                                     
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    XIT                                                              
         CLI   DBFUNCT,DBGETDEM                                                 
         BNE   FILL14                                                           
         MVC   MTHFCTR+2(2),DBDIVSOR                                            
         GOTO1 NBDEMMTH,DMCB,=C'DIV',AIO,DBIO2,MTHBLOCK                         
         SPACE 1                                                                
FILL14   BAS   RE,DEMPOST                                                       
         B     XIT                                                              
         SPACE 1                                                                
DEMHOOK  NTR1                                                                   
         GOTO1 DEFINE,DMCB,=C'TYPE',DBLOCK,WORK                                 
         MVC   POOLPTYP,WORK       PICK UP PROGRAM TYPE                         
         CLI   DBFUNCT,DBGETNTI                                                 
         BE    XIT                                                              
         MVC   MTHFCTR+2(2),DBFACTOR                                            
         GOTO1 NBDEMMTH,DMCB,=C'MAD',AIO,DBIO2,MTHBLOCK                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST ACTUAL DEMO VALUES                               
         SPACE 3                                                                
DEMPOST  NTR1                                                                   
         XC    DEMS,DEMS                                                        
         AI    POOLWEEK,1                                                       
         LA    R1,DBIO2                                                         
         CLI   DBFUNCT,DBGETNTI                                                 
         BNE   *+8                                                              
         L     R1,AIO                                                           
         ST    R1,DBAREC                                                        
         LA    R1,23(R1)                                                        
         ST    R1,DBAQUART                                                      
         MVC   DUB(3),=X'00E201'   GET SHARE                                    
         TM    POOLSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   DUB+1,C'R'          OR RATING IF ESTIMATE WAS RATING             
         GOTO1 NBDEMOUT,DMCB,(C'D',DUB),DBLOCK,DEMS                             
         SPACE 1                                                                
*                                  THEN GET THE VPHS                            
         SPACE 1                                                                
         GOTO1 NBDEMOUT,DMCB,(C'L',NDDEMOS),DBLOCK,DEMS+4                       
         LA    R3,DEMS+2                                                        
         LA    R4,POOLASHR                                                      
         ZIC   R0,NDNDEMOS                                                      
         AH    R0,=H'1'                                                         
         SPACE 1                                                                
DP2      LH    R1,0(R3)                                                         
         AH    R1,0(R4)                                                         
         STH   R1,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DP2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO AVERAGE ACTUAL VALUES PER WEEK                        
         SPACE 3                                                                
AVEPOOL  NTR1                                                                   
         L     R2,APOOL                                                         
         L     R0,POOLNOW                                                       
         SPACE 1                                                                
AVE2     BAS   RE,AVE4             AVERAGE EACH PROGRAM IN POOL                 
         AH    R2,WPOOL                                                         
         BCT   R0,AVE2                                                          
         B     XIT                                                              
         SPACE 1                                                                
AVE4     NTR1                                                                   
         CLI   POOLWEEK,0                                                       
         BE    XIT                                                              
         LA    R3,POOLASHR         SET UP FOR SHARE AND N DEMOS                 
         ZIC   R0,NDNDEMOS                                                      
         AH    R0,=H'1'                                                         
         ZIC   R4,POOLWEEK                                                      
         SPACE 1                                                                
AVE6     LH    RF,0(R3)            PICK UP ACTUAL VALUE                         
         M     RE,=F'2'                                                         
         DR    RE,R4               DIVIDE BY N'WEEKS                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         STH   RF,0(R3)            REPLACE AVERAGE VALUE                        
         LA    R3,4(R3)                                                         
         BCT   R0,AVE6                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO RANK POOL                                            
         SPACE 3                                                                
RANKPOOL NTR1                                                                   
         SPACE 1                                                                
         L     R2,APOOL            FIRST A CORE SORT                            
         L     R3,POOLNOW                                                       
         LH    R4,WPOOL                                                         
         BAS   RE,SEEDPOOL                                                      
         GOTO1 XSORT,DMCB,(1,(R2)),(R3),(R4),4,0                                
         LA    R5,1                                                             
         ST    R5,LASTR5                                                        
         SPACE 1                                                                
RANK2    L     RE,POOLRANK                                                      
         L     RF,POOLRANK(R4)                                                  
         EDIT  (4,LASTR5),(3,POOLRANK)                                          
         MVI   POOLRANK+3,C' '                                                  
         C     R5,LASTR5           IS THIS ITEM THE SAME AS LAST?               
         BE    *+8                                                              
         MVI   POOLRANK+3,C'='                                                  
         CR    RE,RF               IS THIS ITEM THE SAME AS NEXT?               
         BNE   RANK4                                                            
         MVI   POOLRANK+3,C'='                                                  
         B     RANK6                                                            
         SPACE 1                                                                
RANK4    LA    R1,1(R5)                                                         
         ST    R1,LASTR5                                                        
         SPACE 1                                                                
RANK6    AH    R2,WPOOL                                                         
         LA    R5,1(R5)                                                         
         BCT   R3,RANK2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SEED POOL WITH VALUE TO BE RANKED                     
         SPACE 3                                                                
SEEDPOOL NTR1                                                                   
         LA    R3,POOLESHR-POOLRANK DISPLACEMENT TO R3                          
         L     R5,POOLNOW                                                       
         CLI   RANK,C'A'                                                        
         BNE   *+8                                                              
         LA    R3,2(R3)            ADJUST FOR ACTUAL                            
         CLI   RANK+1,C'S'                                                      
         BE    SEED2                                                            
         ZIC   RF,RANK+1           ADJUST FOR DEMOS                             
         SLL   RF,28                                                            
         SRL   RF,26               (*4)                                         
         AR    R3,RF                                                            
         SPACE 1                                                                
SEED2    LA    RF,0(R2,R3)                                                      
         LH    R1,0(RF)                                                         
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,POOLRANK                                                      
         CLI   RANK,C'I'                                                        
         BNE   SEED4                                                            
         SPACE 1                                                                
         XC    POOLRANK,POOLRANK                                                
         LH    R1,2(RF)            COMPUTE INDEX                                
         LTR   R1,R1                                                            
         BZ    SEED4                                                            
         LH    R4,0(RF)                                                         
         LTR   R4,R4                                                            
         BZ    SEED4                                                            
         M     R0,=F'200'                                                       
         DR    R0,R4               ACTUAL DIVIDED BY ESTIMATED                  
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,POOLRANK                                                      
         SPACE 1                                                                
SEED4    AH    R2,WPOOL                                                         
         BCT   R5,SEED2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO REPORT FROM RANKED POOL                              
         SPACE 3                                                                
REPPOOL  NTR1                                                                   
         L     R2,APOOL                                                         
         L     R0,POOLNOW                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
REP2     BAS   RE,REP4                                                          
         AH    R2,WPOOL                                                         
         BCT   R0,REP2                                                          
         B     XIT                                                              
         SPACE 1                                                                
REP4     NTR1                      EDIT A POOLITEM                              
         USING POOLD,R2                                                         
         LA    R3,P                                                             
         A     R3,PDISP                                                         
         MVC   1(4,R3),POOLRANK                                                 
         MVC   6(16,R3),POOLPROG                                                
         MVC   23(4,R3),POOLNET                                                 
         ZIC   R1,POOLDAY                                                       
         MH    R1,=H'3'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   28(3,R3),0(R1)                                                   
         XC    DUB,DUB             ONLY SHOW START                              
         MVC   DUB(2),POOLTIME                                                  
         GOTO1 UNTIME,DMCB,DUB,32(R3)                                           
         MVC   38(6,R3),POOLPCOD                                                
         EDIT  (2,POOLNTI),(4,45(R3)),ALIGN=LEFT                                
         MVC   51(2,R3),POOLPTYP                                                
         EDIT  (1,POOLWEEK),(2,55(R3))                                          
         LA    R2,POOLESHR                                                      
         LA    R3,59(R3)                                                        
         ZIC   R0,NDNDEMOS                                                      
         AH    R0,=H'1'                                                         
         MVC   EAISW,POOLSTAT                                                   
         SPACE 1                                                                
REP6     BAS   RE,EAI              FORMAT EST/ACT/INX                           
         MVI   EAISW,0                                                          
         LA    R2,4(R2)                                                         
         LA    R3,12(R3)                                                        
         BCT   R0,REP6                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT AN EST/ACT/INX EXPRESSION                                 
         SPACE 3                                                                
EAI      NTR1                                                                   
         SPACE 1                                                                
         LH    R1,0(R2)            ESTIMATED                                    
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(3,0(R3))                                                   
         TM    EAISW,X'80'         (SHOW * FOR RATING)                          
         BNO   *+8                                                              
         MVI   3(R3),C'*'                                                       
         SPACE 1                                                                
         LH    R1,2(R2)            ACTUAL                                       
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(3,4(R3))                                                   
         SPACE 1                                                                
         LH    R1,2(R2)            COMPUTE INDEX                                
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         LH    R4,0(R2)                                                         
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         M     R0,=F'200'                                                       
         DR    R0,R4               ACTUAL DIVIDED BY ESTIMATED                  
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         MVI   9(R3),C'*'          SHOW * FOR LARGE NUMBERS                     
         CH    R1,=H'999'                                                       
         BH    XIT                                                              
         EDIT  (R1),(3,8(R3))                                                   
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,49,C'PROGRAM POST REPORT'                                     
         SSPEC H2,49,C'-------------------'                                     
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,45,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,99,PAGE                                                       
         DC    X'00'                                                            
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXCOLS(132),MYCOLS                                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVC   HEAD4(30),RALPH                                                  
         MVC   HEAD8(132),MYHEDA                                                
         MVC   HEAD9(132),MYHEDB                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              STORAGE  LTORG ETC                                               
         SPACE 3                                                                
RELO     DS    A                                                                
APOOL    DS    A                                                                
         SPACE 1                                                                
WPOOL    DC    H'68'                                                            
POOLMAX  DC    F'2000'                                                          
POOLNOW  DC    F'0'                                                             
DAYLIST  DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
SDLIST   DC    X'7C402010080402017F'                                            
EAISW    DC    X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
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
       ++INCLUDE NETDEMOT                                                       
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
**PAN#1  DC    CL21'003NEMED82   05/01/02'                                      
         END                                                                    

*          DATA SET NERES01    AT LEVEL 023 AS OF 01/30/06                      
*PHASE T32101A,*                                                                
T32101   TITLE '-   PURE PROGRAMMING INVENTORY LISTING'                         
T32101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PUED**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         XC    DBEXTRA,DBEXTRA                                                  
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
*                                                                               
VKEY     MVI   PRINTOPT,0                                                       
         LA    R2,PURSRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'60'        ALLOW NAD AND ALL NTI                        
         GOTO1 VVALSRC                                                          
*                                                                               
         LA    R2,PURBOOKH         VALIDATE BOOK                                
         MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         CLI   ACTUAL,1            REALLY ONLY 1 BOOK ALLOWED                   
         BNH   VKEY5                                                            
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
*                                                                               
VKEY5    LA    R2,PURDEMOH         VALIDATE DEMOS                               
         MVI   NUMDEMS,1           PRESET TO 1 DEMO                             
         MVC   DEMO(4),=X'00D90001'  PRESET TO RATING                           
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         MVI   NEDEMTYP,C'4'                                                    
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,8            REALLY ALLOW 8 DEMOS                         
         BNH   VKEY7                                                            
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         EJECT                                                                  
VKEY7    CLI   ACTUAL,2                                                         
         BNH   VKEY10                                                           
         OI    PRINTOPT,X'80'      NOTE - MORE THAN 2 DEMOS                     
*                                                                               
VKEY10   LA    R2,PURNETH          VALIDATE NETWORKS                            
         LA    R3,8                                                             
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
*                                                                               
VKEY20   GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         LA    R4,7(R4)                                                         
VKEY30   BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY20                                                           
         B     VKEY30                                                           
*                                                                               
VKEY40   LA    R2,PUROPTH          VALIDATE OPTIONS                             
         BAS   RE,VALOPT                                                        
         LA    R2,PURTITLH         SET USER'S OWN NAME                          
         GOTO1 VVALTITL                                                         
         LA    R2,PURFILTH         POSSIBLE FILTERS                             
         GOTO1 VVALFILT                                                         
         LA    R2,PURSTIMH         START TIME                                   
         MVC   STIM,=H'600'        DEFAULT                                      
         CLI   5(R2),0                                                          
         BE    VKEY45                                                           
         GOTO1 VVALTIM                                                          
         MVC   STIM,WORK                                                        
         OI    PRINTOPT,X'20'      START TIME FILTER                            
*                                                                               
VKEY45   LA    R2,PURETIMH         END TIME                                     
         MVC   ETIM,=H'545'        DEFAULT                                      
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         GOTO1 VVALTIM                                                          
         MVC   ETIM,WORK                                                        
         OI    PRINTOPT,X'10'      END TIME FILTER                              
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
*                                                                               
VALOPT   NTR1                                                                   
         MVI   GAP,1               PRESET VALUES FOR OPTIONS                    
         MVI   BOXOPT,C'Y'                                                      
         MVI   DAYOPT,C'P'         P=POCKETPIECE                                
         MVI   COROPT,C'N'                                                      
*                                                                               
         MVI   UP,2                DEFAULT IS 2 UP                              
         TM    PRINTOPT,X'80'      BUT, IF MORE THAN 2 DEMOS (X'80')            
         BZ    *+8                                                              
         MVI   UP,1                MUST PRINT 1 UP ON PAGE                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK)                                      
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    BADOPT                                                           
         LA    R3,BLOCK                                                         
*                                                                               
OPT2     CLC   12(3,R3),=C'1UP'                                                 
         BNE   OPT4                                                             
         MVI   UP,1                                                             
         B     OPTEND                                                           
*                                                                               
OPT4     CLC   12(3,R3),=C'BOX'                                                 
         BNE   OPT5                                                             
         MVC   BOXOPT,22(R3)                                                    
         B     OPTEND                                                           
*                                                                               
OPT5     CLC   12(3,R3),=C'DAY'                                                 
         BNE   OPT6                                                             
         MVC   DAYOPT,22(R3)                                                    
         CLI   22(R3),C'A'         ALL DAYS (ROTATORS AND INDIVIDUAL)           
         BE    OPTEND                                                           
         CLI   22(R3),C'I'         INDIVIDUAL DAYS ONLY                         
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPT6     CLC   12(2,R3),=C'S  '                                                 
         BNE   OPT8                                                             
         MVI   GAP,2                                                            
         CLI   22(R3),C'2'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPT8     CLC   12(3,R3),=C'COR'                                                 
         BNE   OPT10                                                            
         MVC   COROPT,22(R3)                                                    
         CLI   22(R3),C'Y'         YES IS ONLY POSSIBILITY                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPT10    B     BADOPT                                                           
         EJECT                                                                  
OPTEND   LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
*                                                                               
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYEND                                                            
         EJECT                                                                  
*              PRINT REPORT - INITIALIZE                                        
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   RESTITLE,=CL40'PURE PROGRAMMING LISTING'                         
         MVI   MAXLINES,59                                                      
         MVI   COUNT,0             COUNT NUMBER OF LINES ON PAGE                
*                                                                               
         LA    R2,BUFF             CLEAR OUT BUFFER AREA                        
         LA    R3,48                                                            
*                                                                               
CLEAR    MVC   0(110,R2),SPACES                                                 
         LA    R2,110(R2)                                                       
         BCT   R3,CLEAR                                                         
*                                                                               
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         MVC   DBAUTH,TWAAUTH                                                   
         MVI   DATATYP,C'P'        SET UP DATA TYPE - EITHER PURE               
         MVI   DBDEMTYP,C'4'                                                    
         MVC   DBSELTIM(2),STIM                                                 
         MVC   DBSELTIM+2(2),ETIM                                               
         CLC   PURSRCE(3),=C'NHI'                                               
         BNE   *+14                                                             
         MVC   DBFILE,=C'NTI'                                                   
         B     SETDAY                                                           
*                                                                               
         CLC   PURSRCE(3),=C'NAD'  1-29-03 SEAN                                 
         BNE   *+12                                                             
         LA    R1,PURSRCE+3                                                     
*                                                                               
         B     SRC10                                                            
         CLC   PURSRCE(3),=C'WB1'                                               
         BNE   *+12                                                             
         LA    R1,PURSRCE+3        POINT R1 TO NEXT FIELD                       
         B     SRC10                                                            
         CLC   PURSRCE(3),=C'NTI'                                               
         BNE   SETDAY                                                           
         LA    R1,PURSRCE+3                                                     
*                                                                               
SRC10    MVI   DATATYP,C'N'        OR NTI NUMBER                                
         MVC   DBDAYOPT,DAYOPT     P, A, OR I                                   
*        CLC   PURSRCE+3(2),=C',N' USED TO TEST FOR NTI,N                       
         CLC   0(2,R1),=C',N'                                                   
         BE    SETNTI                                                           
*        CLC   PURSRCE+3(2),=C',D' USED TO TEST FOR NTI,D                       
         CLC   0(2,R1),=C',D'                                                   
         BE    SETDAY                                                           
         MVI   DBFUNCT,DBGETDEM    DEFAULT IS TIME PERIOD NTI                   
         MVI   DBBEST,C'A'         SET TO ALL TO RETURN OVERLAPS                
         CLC   NETSAVE(5),=C'HUT H'                                             
         BNE   CONIO                                                            
         MVI   DBSELDAY,X'7F'                                                   
         B     CONIO                                                            
*                                                                               
SETNTI   XC    DBSELTIM,DBSELTIM   ,N GIVES NTI SEQUENCE                        
         B     CONIO                                                            
*                                                                               
SETDAY   MVI   DBSELDAY,X'7F'      ,D GIVES DAY/TIME SEQUENCE                   
         EJECT                                                                  
*              CONTROL I/O                                                      
*                                                                               
CONIO    LA    R2,PURNETH          NETWORKS ON SCREEN                           
         LA    R3,8                UP TO 8 NETWORKS                             
         LA    R5,NETSAVE          LIST OF NETWORKS IN DBSELSTA FMT             
*                                                                               
*                                  BUILD REST OF DBLOCK                         
PURE     MVC   DBSELSTA,0(R5)                                                   
         GOTO1 VADJSEL             ADJUST FOR BOOK TYPE                         
         GOTO1 VDISPNET            GET EDITED NETWORK IN WORK                   
         MVC   THISNET,WORK                                                     
         MVC   DBSELBK,BOOK+1                                                   
*                                                                               
         CLC   PURSRCE(3),=C'WB1'  TCAR REQUEST?                                
         BNE   *+8                                                              
         MVI   DBBTYPE,C'V'        OVER-RIDING (ADJSEL) FOR TCAR                
*                                                                               
         MVC   SVDBLOCK,DBLOCKA    SAVE DBLOCK                                  
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL                                         
         CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BNE   PURE2               SKIP LISTING                                 
         OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    PURE2               NO                                           
         BAS   RE,SPLAT                                                         
         MVC   PAGE,=H'1'                                                       
         BCT   R3,*+8                                                           
         B     XIT                                                              
*                                                                               
PURE2    BAS   RE,BUMP             NEXT NETWORK ON SCREEN                       
         LA    R5,7(R5)            NEXT DBSELSTA FORMATTED NETWORK              
         OC    0(5,R5),0(R5)       ANY MORE?                                    
         BZ    XIT                 NO                                           
         MVC   DBLOCKA,SVDBLOCK    RESTORE DBLOCKA                              
         B     PURE                GET NEXT NETWORK                             
         EJECT                                                                  
*              PROCESS A RECORD                                                 
*                                                                               
FILL     NTR1                                                                   
         XC    DUB,DUB             CHECK FOR FILTERS                            
         GOTO1 DEFINE,PARAS,=C'TYPE',DBLOCKD,WORK                               
         MVC   DUB(4),WORK                                                      
         GOTO1 VCHEFILT,DMCB,DUB                                                
         BNE   XIT                                                              
         ZIC   R5,COUNT                                                         
         LA    R3,BUFF+1                                                        
         CLI   UP,2                                                             
         BNE   FILL10                                                           
         CLI   COUNT,93                                                         
         BNL   FILL20                                                           
         CLI   COUNT,48                                                         
         BL    FILL30                                                           
         LA    R3,55(R3)                                                        
         SH    R5,=H'48'                                                        
         B     FILL30                                                           
*                                                                               
FILL10   CLI   COUNT,45                                                         
         BL    FILL30                                                           
FILL20   BAS   RE,SPLAT            OUTPUT A PAGE                                
         SR    R5,R5                                                            
*                                                                               
FILL30   ZIC   R1,COUNT            ADJUST COUNT                                 
         ZIC   R0,GAP                                                           
         AR    R1,R0                                                            
         STC   R1,COUNT                                                         
         MH    R5,=H'110'          BUMP DOWN BY LINE COUNT                      
         AR    R3,R5                                                            
*                                                                               
         CLC   PURSRCE(5),=C'NTI,D' USE EXACT TIME FOR NTI,D                    
         BNE   FILL34                                                           
         GOTO1 DEFINE,PARAS,=C'TIMX',DBLOCKD,WORK                               
         B     FILL36                                                           
FILL34   GOTO1 DEFINE,PARAS,=C'TIME',DBLOCKD,WORK                               
FILL36   XC    DUB,DUB                                                          
         MVC   DUB(2),WORK+2       EXTRACT START TIME                           
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,(R1),DUB,WORK                                             
         CLC   LASTIME,WORK                                                     
         BE    *+16                                                             
         MVC   0(6,R3),WORK        SHOW TIME IF DIFFERENT                       
         MVC   LASTIME,WORK                                                     
*                                                                               
         CLI   COROPT,C'Y'         TEST NEED FOR CORRECTION IND.                
         BNE   FILL38                                                           
         GOTO1 DEFINE,PARAS,=C'CORR',DBLOCKD,WORK                               
         MVC   6(1,R3),WORK        SET ADD OR CHANGE                            
         EJECT                                                                  
FILL38   GOTO1 DEFINE,PARAS,=C'DAY',DBLOCKD,WORK                                
         MVC   13(3,R3),WORK+2     EXTRACT 3 BYTE ALPHA DAY                     
         GOTO1 (RF),(R1),=C'PURE',DBLOCKD,WORK                                  
*                                                                               
         MVC   7(4,R3),WORK+3      EXTRACT EDITED PURE NUMBER                   
         CLI   DATATYP,C'N'        OPTION TO SHOW NTI NUMBER                    
         BNE   FILL40                                                           
         GOTO1 DEFINE,PARAS,=C'NTI',DBLOCK,WORK                                 
         MVC   7(5,R3),WORK                                                     
*                                                                               
FILL40   GOTO1 (RF),(R1),=C'PROGRAM',DBLOCKD,WORK                               
         OC    WORK(16),SPACES                                                  
         MVC   17(16,R3),WORK                                                   
         GOTO1 (RF),(R1),=C'EPISODE',DBLOCKD,WORK                               
         CLC   WORK(16),SPACES                                                  
         BE    FILL50                                                           
         MVC   110+16(16,R3),WORK  EPISODE TITLE ON 2ND LINE                    
         ZIC   R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
*                                                                               
FILL50   CLI   DAYOPT,C'P'         TEST POCKETPIECE                             
         BNE   FILL60                                                           
         CLC   PURSRCE(5),=C'NTI,D' TEST NTI,D                                  
         BE    FILL60                                                           
         GOTO1 DEFINE,PARAS,=C'ADYT',DBLOCKD,DYTM                               
         OC    DYTM+10(10),DYTM+10 TEST INDIVIDUAL DAYS                         
         BZ    FILL60                                                           
         BAS   RE,SHOWDAYS         SHOW INDIVIDUAL DAYS' TIMES                  
*                                                                               
FILL60   MVC   SAVBK,DBACTBK                                                    
         ZIC   R1,NUMDEMS                                                       
         MH    R1,=H'4'                                                         
         LA    R1,DEMOS(R1)                                                     
         MVI   0(R1),X'FF'                                                      
         LA    R2,DEMOS                                                         
         LA    R6,33(R3)                                                        
         CLI   DBSELSTA+4,C'C'                                                  
         BNE   FILL61                                                           
         LA    RE,DBEXTRA          FORCE 2 CHAR RATINGS                         
         USING DBXNTID,RE                                                       
         ST    RE,DBEXTEND                                                      
         MVI   DBXNCR2,C'Y'                                                     
         MVC   DBXNID,=C'NETW'                                                  
FILL61   CLC   PURSRCE(3),=C'WB1'  TCAR?                                        
         BNE   *+8                                                              
         MVI   DBBTYPE,C'V'        TELL IT TO DEMOUT                            
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,WORKAREA                         
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DBACTBK,SAVBK       RESTORE IT                                   
         LA    R5,WORKAREA         NOW POINT R5 TO VALUES                       
         ZIC   R1,NUMDEMS                                                       
         EJECT                                                                  
FILL70   EDIT  (4,0(R5)),(8,0(R6)),2   2 DECIMAL POINT FOR                      
         MVC   PUDEMMOD,1(R2)                                                   
         CLI   DBSELSTA+4,C'C'          CABLE                                   
         BNE   FILL71                                                           
         CLI   PUDEMMOD,C'R'            RATINGS                                 
         BE    FILL80                                                           
FILL71   EDIT  (4,0(R5)),(8,0(R6)),1   1 DECIMAL POINT FOR                      
         CLI   PUDEMMOD,C'R'       RATING                                       
         BE    FILL80                                                           
         CLI   PUDEMMOD,C'P'       PUT                                          
         BE    FILL80                                                           
         CLI   PUDEMMOD,C'S'       SHARE                                        
         BE    FILL80                                                           
         CLI   PUDEMMOD,C'O'       TP PUT                                       
         BE    FILL80                                                           
         CLI   PUDEMMOD,C'Q'       TP SHARE                                     
         BE    FILL80                                                           
         CLI   PUDEMMOD,C'L'       GAA RTG                                      
         BE    FILL80                                                           
         CLI   PUDEMMOD,C'X'       TSA                                          
         BE    FILL80                                                           
         EDIT  (4,0(R5)),(8,0(R6))   ALL ELSE, INTEGER                          
*                                                                               
FILL80   LA    R2,4(R2)            NEXT DEMO                                    
         LA    R6,9(R6)            NEXT DEMO AREA                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R1,FILL70                                                        
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*              OUTPUT A PAGE                                                    
*                                                                               
SPLAT    NTR1                                                                   
         LA    R2,BUFF                                                          
         LA    R3,48                                                            
*                                                                               
SPLAT20  CLC   0(110,R2),SPACES                                                 
         BNE   SPLAT30                                                          
         CLI   GAP,2               ADDITIONAL SPACE                             
         BNE   SPLAT40                                                          
SPLAT30  MVC   P(110),0(R2)                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
SPLAT40  MVC   0(110,R2),SPACES                                                 
         LA    R2,110(R2)                                                       
         BCT   R3,SPLAT20                                                       
         MVI   COUNT,0                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              SHOW DISTINCT START TIMES FOR INDIVIDUAL DAYS                    
*                                                                               
SHOWDAYS NTR1                                                                   
         LA    R2,DYTM+10                                                       
         LA    R4,7                                                             
SD20     OC    0(10,R2),0(R2)                                                   
         BZ    SDX                 NO DIFFERENCE IN START TIMES                 
         CLC   2(2,R2),DYTM+2      TEST START TIME AGAINST ROTATOR              
         BNE   SD30                                                             
         LA    R2,10(R2)                                                        
         BCT   R4,SD20                                                          
         B     SDX                 NO DIFFERENCE IN START TIMES                 
*                                                                               
SD30     LA    R6,110(R3)          USE 2ND AND 3RD LINES                        
         CLC   16(16,R6),SPACES    TEST WHETHER EPISODE PRESENT                 
         BE    *+8                                                              
         LA    R6,110(R6)          USE 3RD AND 4TH LINES                        
         LA    R2,DYTM+10                                                       
         LA    R4,7                                                             
SD40     OC    0(10,R2),0(R2)                                                   
         BZ    SD90                                                             
         MVC   12(3,R6),7(R2)      DAY ON LINE 1                                
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),2(R2)        EXTRACT START TIME                           
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,(R1),DUB,WORK                                             
         MVC   110+12(6,R6),WORK   TIME ON LINE 2                               
*                                                                               
         LA    R2,10(R2)           NEXT DAY                                     
         LA    R6,6(R6)                                                         
         BCT   R4,SD40                                                          
*                                                                               
SD90     ZIC   R1,COUNT                                                         
         LA    R1,2(R1)                                                         
         STC   R1,COUNT                                                         
*                                                                               
SDX      B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK - CONTROL                                          
*                                                                               
HOOK     NTR1                                                                   
         BAS   RE,HOOKTIME                                                      
         BAS   RE,HOOKHEAD                                                      
         BAS   RE,HOOKBOX                                                       
         GOTO1 VRESHEAD                                                         
*                                  PRINT COPYRIGHT INFO                         
         MVC   WORK(40),=CL40'RTG/IMP-(C) YYYY NIELSEN MEDIA'                   
         GOTO1 DATCON,DMCB,(5,DUB),(20,DUB)                                     
         MVC   WORK+12(4),DUB      PUT CURRENT YEAR HERE                        
         GOTO1 CENTER,DMCB,WORK,40 CENTERED                                     
         LA    R2,H4+30                                                         
         CLI   HOWWIDE,132         MAY BE WIDE PRINT                            
         BNE   *+8                                                              
         LA    R2,H4+40                                                         
         MVC   0(40,R2),WORK                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - SHOW REQUESTED START AND END TIMES                        
*                                                                               
HOOKTIME NTR1                                                                   
         LA    R3,H6                                                            
         TM    PRINTOPT,X'20'      START TIME ENTERED?                          
         BZ    HT10                                                             
         XC    DUB,DUB                                                          
         MVC   DUB(2),STIM                                                      
         MVC   0(12,R3),=C'START TIME -'                                        
         GOTO1 UNTIME,PARAS,DUB,13(R3)                                          
         LA    R3,21(R3)                                                        
*                                                                               
HT10     TM    PRINTOPT,X'10'      END TIME ENTERED?                            
         BZ    XIT                                                              
         XC    DUB,DUB                                                          
         MVC   0(10,R3),=C'END TIME -'                                          
         MVC   DUB(2),ETIM                                                      
         GOTO1 UNTIME,PARAS,DUB,11(R3)                                          
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - DEAL WITH HEADINGS                                        
*                                                                               
HOOKHEAD NTR1                                                                   
         MVC   RESTITA,SPACES                                                   
         MVC   RESTITB,SPACES                                                   
         MVC   RESTITC,SPACES                                                   
         MVC   RESTITA(25),=C'  TIME  PURE  DAY PROGRAM'                        
         MVC   RESTITB+9(3),=C'NO.'                                             
         CLC   PURSRCE(3),=C'WB1'  TCAR OR NO?                                  
         BNE   *+10                                                             
         MVC   RESTITC+9(4),=C'CAT.'                                            
         CLI   DATATYP,C'N'                                                     
         BNE   *+10                                                             
         MVC   RESTITA+8(4),=C'NTI.'                                            
*                                                                               
         LA    R3,RESTITA+34       NOW POINT TO WHERE                           
         LA    R5,RESTITB+34       1ST DEMO SHOULD PRINT                        
         LA    R2,DEMOS                                                         
         ZIC   R6,NUMDEMS                                                       
*                                                                               
HH2      CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
*        BNE   *+8                                                              
         B     *+8                                                              
         MVI   1(R2),C'I'                                                       
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         MVI   DBDEMTYP,C'4'                                                    
         GOTO1 DEMOCON,PARAS,(0,(R2)),(7,WORK),(0,DBLOCKD)                      
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         CLI   WORK+5,C' '         CHECK LENGTH OF LITERAL                      
         BH    *+14                                                             
         MVC   2(5,R3),WORK        CENTER SHORT LITERAL (E.G.,'HOMES')          
         B     *+10                                                             
         MVC   1(7,R3),WORK        FORMAT (FOR 9 POSITION COLUMN)               
         MVC   2(5,R5),WORK+7                                                   
         CLI   2(R5),C'*'                                                       
         BNE   *+10                                                             
         MVC   2(5,R5),=C'(000)'                                                
*                                                                               
         CLC   PURSRCE(3),=C'WB1'        TCAR?                                  
         BNE   HH3                                                              
         CLI   WORK+5,C' '                                                      
         BH    *+14                                                             
         MVC   L'RESTITC+2(7,R5),WORK+12 NEED CATOGORY NAMER                    
         B     *+10                                                             
         MVC   L'RESTITC+1(7,R5),WORK+12 ALIGN WITH DEMO NAME                   
*                                                                               
HH3      LA    R2,3(R2)                                                         
         CLI   NEDEMTYP,C'4'                                                    
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R3,9(R3)                                                         
         LA    R5,9(R5)                                                         
         BCT   R6,HH2                                                           
         CLI   UP,2                                                             
         BNE   XIT                                                              
         MVC   RESTITA+55(55),RESTITA                                           
         MVC   RESTITB+55(55),RESTITB                                           
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - BOXES                                                     
*                                                                               
HOOKBOX  NTR1                                                                   
         MVC   RESCOLS,SPACES                                                   
         LA    R2,RESCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   07(R2),C'C'                                                      
         MVI   13(R2),C'C'                                                      
         MVI   17(R2),C'C'                                                      
*                                                                               
         LA    R2,RESCOLS+34                                                    
         ZIC   R3,NUMDEMS          NOW SET UP FOR DEMOS                         
*                                                                               
HB2      MVI   0(R2),C'C'                                                       
         LA    R2,9(R2)            9 POSITION COLUMN                            
         BCT   R3,HB2                                                           
         MVI   0(R2),C'R'                                                       
         CLI   UP,2                PROLIFERATE IF 2 UP                          
         BNE   XIT                                                              
         MVC   RESCOLS+55(55),RESCOLS                                           
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
*                                                                               
ERREND   GOTO1 VERRXIT                                                          
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
SVDBLOCK DS    CL(L'DBLOCK)                                                     
*                                                                               
*              CONSTANTS                                                        
*                                                                               
DYTM     DC    XL80'00'            DAY/TIMES FOR INDIVIDUAL DAYS                
*                                                                               
MANYBKS  DC    C'** ERROR ** TOO MANY BOOKS - LIMIT IS 1'                       
MANYDEM  DC    C'** ERROR ** TOO MANY DEMOS - LIMIT IS 11'                      
OPTERR   DC    C'** ERROR ** INVALID OPTION '                                   
LTRTAB   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* NERESALL                                                                      
         PRINT OFF                                                              
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
*                                                                               
*              DSECT TO COVER SCREEN                                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF1D                                                       
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
UP       DS    CL1                 1 UP OR 2 ON PAGE                            
LASTIME  DS    CL6                 LAST TIME PRINTED                            
PRINTOPT DS    XL1                 X'80'  MORE THAN 2 DEMOS                     
*                                  X'40'  UNUSED                                
*                                  X'20'  START TIME FILTER                     
*                                  X'10'  END TIME FILTER                       
DATATYP  DS    CL1                 P(URE) OR N(TI)                              
PUDEMMOD DS    C                   ALPHA DEMO MODIFIER                          
DBEXTRA  DS    CL128                                                            
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
       ++INCLUDE DEDBEXTRAD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023NERES01   01/30/06'                                      
         END                                                                    

*          DATA SET SPRES75    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T20F75A,+0                                                               
         TITLE 'T20F75 - EDIT FOR SPOT OVERNIGHT RESEARCH'                      
T20F75   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F75**,RR=R8                                                 
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T20F75+4096,RA                                                   
         ST    R8,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING T20FFFD,R3                                                       
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   EXIT                                                             
         SPACE 1                                                                
         MVI   DOWNOPT,C'N'                                                     
         MVI   BOXOPT,C'Y'         PRESET FIELDS                                
         MVI   REPAVE,C'N'                                                      
         MVI   QRTOPT,C'N'                                                      
         XC    DAYTMLST,DAYTMLST                                                
         MVI   NDAYTMS,0                                                        
         XC    FILTERS,FILTERS                                                  
         XC    USERTTL,USERTTL                                                  
         XC    DPLIST,DPLIST                                                    
         XC    PTLIST,PTLIST                                                    
         MVC   DEMOS(24),DEFDEM                                                 
         MVI   NDEMOS,10           SET FOR MAX 10 DEMOS                         
         MVI   SPACOPT,1                                                        
         XC    UPOPT,UPOPT                                                      
         XC    BOOKOPT,BOOKOPT                                                  
         XC    AESTBK,AESTBK                                                    
         MVI   DPOPT,C'N'                                                       
         MVI   AVOPT,C'N'                                                       
         XC    TOPOPT,TOPOPT                                                    
         MVI   REPOPT,C'N'                                                      
         MVI   SPECOPT,C'Y'                                                     
         XC    MINOPT,MINOPT                                                    
         XC    MAXOPT,MAXOPT                                                    
         MVI   WEEKOPT,0                                                        
         MVI   CPPOPT,0                                                         
         MVI   SIDOPT,0                                                         
         XC    SIDSTAT,SIDSTAT                                                  
         XC    STATS,STATS                                                      
         MVI   SIDPER,0                                                         
         MVI   SIDUPOPT,0                                                       
         MVI   STACKOPT,0                                                       
         MVI   SURVOPT,0                                                        
         MVI   OVEROPT,0                                                        
         MVI   COMPOPT,0                                                        
         MVI   EFFOPT,0                                                         
         MVI   WDEMS,8             SET DEFAULT DEMO COL WIDTH                   
         MVI   MULTIOPT,C'N'                                                    
         MVI   MGROPT,C'N'                                                      
         MVI   RTGOPT,C'N'         SET FOR 1 YEAR RTGS/HUTS                     
         MVI   PUTOPT,C'N'                                                      
         MVI   EBOPT,C'N'                                                       
* READ R0 PROFILE                                                               
         L     RE,ASPOOLD                                                       
         LA    R4,PROGPROF-SPOOLD(RE)                                           
         XC    0(16,R4),0(R4)      CLEAR PROFILE                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0R0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,WORK,(R4),DATAMGR                                   
         B     EDIT                                                             
         SPACE 1                                                                
DEFDEM   DC    X'00',C'R',AL1(01)                                               
         DC    X'00',C'T',AL1(01)                                               
         DC    X'00',C'T',AL1(41)                                               
         DC    X'00',C'T',AL1(47)                                               
         DC    X'00',C'T',AL1(45)                                               
         DC    X'00',C'T',AL1(91)                                               
         DC    X'00',C'T',AL1(97)                                               
         DC    X'00',C'T',AL1(95)                                               
         EJECT                                                                  
* EDIT SOURCE AND BOOK(S)                                                       
         SPACE 1                                                                
EDIT     DS    0H                                                               
         TM    WHEN,X'20'          TEST SOON REQUEST                            
         BZ    EDIT2                                                            
         CLI   OFFLINE,C'Y'                                                     
         BE    EDIT2                                                            
         CLI   T20FFFD+1,C'*'      TEST DDS TERMINAL                            
         BE    EDIT2                                                            
         LA    R2,CONWHENH                                                      
         MVI   ERROR,INVPRINT                                                   
         B     EDTERR                                                           
*                                                                               
EDIT2    LA    R2,RESSRCEH         SOURCE                                       
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
EDIT4    LA    R2,RESBOOKH         BOOK(S)                                      
         GOTO1 VVALBOOK                                                         
*                                                                               
EDIT6    LA    R1,BOOKS                                                         
         ZIC   R0,NBOOKS                                                        
*                                                                               
EDIT8    TM    0(R1),X'20'                                                      
         BNO   *+8                                                              
         ST    R1,AESTBK           NOTE ESTIMATED BOOK ADDRESS                  
         LA    R1,4(R1)                                                         
         BCT   R0,EDIT8                                                         
         B     EDIT10                                                           
         EJECT                                                                  
* EDIT STATIONS                                                                 
         SPACE 1                                                                
EDIT10   LA    R2,RESOPTH          VALIDATE OPTIONS FIRST IN                    
         BAS   RE,EDOPT            CASE SID IS SPECIFIED                        
         BNE   EDTERR                                                           
*                                                                               
         LA    R2,RESSTATH         STATIONS                                     
         GOTO1 ANY                 MUST BE 1                                    
*                                                                               
         CLC   =C'ALL',8(R2)       ALLOW ALL FOR MKTGRPS AND SID                
         BNE   EDIT11                                                           
         MVI   NSTATS,1                                                         
         MVC   STATS(5),=C'ALL  '                                               
         B     EDIT20                                                           
*                                                                               
EDIT11   LA    R4,STATS                                                         
         LA    R5,1                SET COUNTER                                  
         LA    R6,7                AND MAX                                      
         SPACE 1                                                                
EDIT12   DS    0H                                                               
         GOTO1 VVALSTA                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         MVC   5(2,R4),ACTMKT                                                   
*                                                                               
         OC    ACTSTAT,ACTSTAT     MARKET EXPRESSIONS                           
         BNZ   EDIT14                                                           
         CLI   SIDOPT,C'Y'         ONLY ALLOWED WITH SID                        
         BE    EDIT14                                                           
         MVI   ERROR,INVSTAT                                                    
         B     EDTERR                                                           
         SPACE 1                                                                
EDIT14   STC   R5,NSTATS                                                        
         BAS   RE,BUMP             NEXT STATION FIELD                           
         LA    R4,7(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,*+8                                                           
         B     EDIT20                                                           
         CLI   5(R2),0                                                          
         BNE   EDIT12                                                           
         B     EDIT20                                                           
         EJECT                                                                  
* EDIT DAY/TIME COMBINATIONS                                                    
         SPACE 1                                                                
EDIT20   LA    R2,RESDAYH          DAY/TIME LIST                                
         LA    R4,DAYTMLST                                                      
         MVI   0(R4),X'FF'         PRESET FOR ALL                               
         MVI   LASTDAY,0                                                        
         LA    R5,1                SET COUNTER                                  
         LA    R6,6                AND MAX                                      
         SPACE 1                                                                
EDIT22   CLI   5(R2),0                                                          
         BE    EDIT24                                                           
         MVI   0(R4),X'FF'                                                      
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT26                                                           
         GOTO1 VVALDAY                                                          
         MVC   0(1,R4),ACTUAL                                                   
         B     EDIT26                                                           
         SPACE 1                                                                
EDIT24   BAS   RE,BUMP             IF DAY IS OMITTED                            
         CLI   5(R2),0             IS THERE ANY TIME                            
         BE    EDIT26                                                           
         MVC   0(1,R4),LASTDAY                                                  
         B     EDIT28                                                           
         SPACE 1                                                                
EDIT26   BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    EDIT40                                                           
         SPACE 1                                                                
EDIT28   CLC   8(3,R2),=C'ALL'                                                  
         BE    EDIT28X                                                          
         GOTO1 VVALTIM                                                          
         MVC   1(4,R4),WORK                                                     
         SPACE 1                                                                
EDIT28X  STC   R5,NDAYTMS                                                       
         MVC   LASTDAY,0(R4)                                                    
         BAS   RE,BUMP                                                          
         LA    R4,5(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,EDIT22                                                        
         B     EDIT40                                                           
         EJECT                                                                  
* EDIT DETAILS, AVERAGES AND DEMOS                                              
         SPACE 1                                                                
EDIT40   LA    R2,RESDETH          DETAILS                                      
         BAS   RE,VALIDET                                                       
         SPACE 1                                                                
         LA    R2,RESAVEH          AVERAGES                                     
         BAS   RE,VALIAVE                                                       
         SPACE 1                                                                
         LA    R2,RESDEMOH         DEMOS                                        
         GOTO1 VVALDEM                                                          
         SPACE 1                                                                
         LA    R2,RESFILTH         NOW FILTERS                                  
         BAS   RE,EDITFILT                                                      
*                                                                               
         LA    R2,RESTTLH                                                       
         CLI   5(R2),0                                                          
         BE    EDIT44                                                           
         MVI   USERTTL,C' '                                                     
         MVC   USERTTL+1(L'USERTTL-1),USERTTL                                   
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   USERTTL(0),8(R2)                                                 
*                                                                               
EDIT44   DS    0H                                                               
         B     EDIT50                                                           
         EJECT                                                                  
* FINAL LOGICAL VALIDATION ROUTINES *                                           
         SPACE 1                                                                
EDIT50   LA    R2,RESSTATH                                                      
         CLC   =C'ALL',STATS       TEST ALL STATION REQUEST                     
         BNE   EDIT52                                                           
         CLI   MGROPT,C'Y'         TEST MKTGRP REQUEST                          
         BE    EDIT52                                                           
         CLI   SIDOPT,C'Y'         OR SID                                       
         BE    EDIT52                                                           
         MVI   ERROR,INVSTAT                                                    
         B     EDTERR                                                           
*                                                                               
EDIT52   CLI   SIDOPT,C'Y'         IF SID OPTION ON                             
         BNE   EDIT54                                                           
         CLI   SIDPER,0            MUST SPECIFY PERIOD                          
         BNZ   EDIT54                                                           
         CLI   NSIDPER,0                                                        
         BNZ   EDIT54                                                           
         MVI   ERROR,NOSIDPER                                                   
         LA    R2,RESOPTH                                                       
         B     EDTERR                                                           
*                                                                               
EDIT54   CLI   MGROPT,C'Y'         TEST MKTGRPS                                 
         BNE   EDIT56                                                           
         CLI   SIDOPT,C'Y'         TEST SID                                     
         BE    EDIT54A                                                          
         MVI   ERROR,NOMGRSID      SHOULD HAVE SID                              
         LA    R2,RESOPTH                                                       
         B     EDTERR                                                           
*                                                                               
EDIT54A  LA    R4,DETS             MKTGRP REQUIRES STATION TO PRINT             
         ZIC   R5,NDETS                                                         
*                                                                               
EDIT54B  CLI   1(R4),DTLEQSTA                                                   
         BE    EDIT56                                                           
         LA    R4,4(R4)                                                         
         BCT   R5,EDIT54B                                                       
         MVI   ERROR,NOSTADET                                                   
         LA    R2,RESDETH                                                       
         B     EDTERR                                                           
*                                                                               
EDIT56   B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**************************************************                              
* EDIT OPTIONS                                   *                              
* NOTE THAT R4 POINTS TO SCANNER BLOCK           *                              
*           R5 HAS COUNT OF NUMBER OF OPTIONS    *                              
**************************************************                              
         SPACE 1                                                                
EDOPT    NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    YES                                                              
         MVI   SCANLEN,42          SET TABLE ENTRY LENGTH                       
         BAS   RE,CLRBLOCK                                                      
         GOTO1 SCANNER,DMCB,(20,(R2)),(10,BLOCK),0                              
*                                                                               
         LA    R4,BLOCK                                                         
         SR    R5,R5                                                            
         ICM   R5,1,4(R1)                                                       
         BZ    SCANERR                                                          
         SPACE 1                                                                
EDOPT2   CLC   12(2,R4),=C'S '     SPACING                                      
         BNE   EDOPT4                                                           
         CLI   11(R4),0                                                         
         BE    SCANERR                                                          
         MVC   SPACOPT,11(R4)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT4   CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   EDOPT6                                                           
         MVC   BOXOPT,22(R4)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT6   CLC   12(7,R4),=C'QUARTER'  OPTION TO SHOW QUARTER HOUR                
         BNE   EDOPT8                                                           
         MVI   QRTOPT,C'Y'                                                      
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT8   CLC   12(4,R4),=C'BOOK'   BOOK FOR UPGRADE                             
         BE    *+14                                                             
         CLC   12(2,R4),=C'BK'                                                  
         BNE   EDOPT10                                                          
         XC    WORK,WORK                                                        
         MVC   WORK+5(1),1(R4)                                                  
         MVC   WORK+8(10),22(R4)                                                
         LA    R6,BOOKOPT                                                       
         GOTO1 BOOKVAL,DMCB,(DBSELSRC,WORK),(1,(R6)),(C'S',SCANNER)             
         CLI   4(R1),1                                                          
         BNE   SCANERR                                                          
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT10  CLC   12(2,R4),=C'UP..'   UPGRADE                                      
         BNE   EDOPT16                                                          
         MVI   DPOPT,C'Y'          FORCE ON DAYPART OPTION                      
         MVI   UPFILE,C'T'         DEFAULT IS TP                                
         CLI   14(R4),C' '                                                      
         BE    *+10                                                             
         MVC   UPFILE,14(R4)                                                    
         MVC   UPOPT(3),22(R4)                                                  
         CLC   UPOPT(3),=C'SID'    OPTION TO COME FROM SID                      
         BNE   EDOPT12                                                          
         MVI   SIDUPOPT,C'Y'                                                    
         MVI   SIDOPT,C'Y'                                                      
         B     EDOPT14                                                          
         SPACE 1                                                                
EDOPT12  XC    WORK,WORK                                                        
         MVC   WORK+5(1),1(R4)                                                  
         MVC   WORK+8(20),22(R4)                                                
         XC    DMCB(12),DMCB       GET THE ADDRESS OF UPVAL                     
         MVC   DMCB+4(4),=X'D9000A13' (T00A13)                                  
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,WORK,UPOPT,(C'/',ACOMFACS)                             
         CLI   0(R1),0                                                          
         BE    SCANERR                                                          
         SPACE 1                                                                
EDOPT14  OC    AESTBK,AESTBK       MUST BE ESTIMATED BOOK                       
         BNZ   EDITEND                                                          
         MVI   ERROR,NOESTBK                                                    
         GOTO1 VCURSERR                                                         
         SPACE 1                                                                
EDOPT16  CLI   0(R4),2             AT LEAST TWO CHARS                           
         BL    EDOPT18             NO                                           
         ZIC   RE,0(R4)            GET LENGTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'DAYPART' DAYPART GROUPING                            
         BNE   EDOPT18                                                          
         MVI   DPOPT,C'Y'                                                       
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT18  CLC   12(5,R4),=C'AVAIL'  AVAIL FORMAT                                 
         BNE   EDOPT20                                                          
         MVI   AVOPT,C'Y'                                                       
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT20  CLC   12(3,R4),=C'TOP'    SHOW ONLY TOP NNN                            
         BNE   EDOPT22                                                          
         OC    8(4,R4),8(R4)                                                    
         BZ    SCANERR                                                          
         MVC   TOPOPT,8(R4)                                                     
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT22  CLC   12(3,R4),=C'MAX='   DONT SHOW ABOVE MAX                          
         BNE   EDOPT24                                                          
         BAS   RE,VALCASH                                                       
         BNE   SCANERR                                                          
         MVC   MAXOPT,WORK                                                      
         LA    R1,MAXCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT24  CLC   12(3,R4),=C'MIN='   DONT SHOW BELOW MIN                          
         BNE   EDOPT26                                                          
         BAS   RE,VALCASH                                                       
         BNE   SCANERR                                                          
         MVC   MINOPT,WORK                                                      
         LA    R1,MINCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT26  CLC   12(4,R4),=C'WEEK'   CONTROL OF WEEK DATA                         
         BNE   EDOPT28                                                          
         MVI   WEEKOPT,X'FF'                                                    
         CLC   22(3,R4),=C'ALL'    SHOW ALL                                     
         BE    EDITEND                                                          
         MVC   WEEKOPT,11(R4)      OR WEEKS 1-4                                 
         CLI   WEEKOPT,0                                                        
         BE    SCANERR                                                          
         CLI   WEEKOPT,4                                                        
         BH    SCANERR                                                          
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT28  CLC   12(4,R4),=C'CPP '   CPP OPTION                                   
         BNE   EDOPT30                                                          
         MVI   CPPOPT,C'Y'                                                      
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT30  CLC   12(4,R4),=C'CPM '   (ACCEPT CPM AS SYNONYM)                      
         BNE   EDOPT32                                                          
         MVI   CPPOPT,C'Y'                                                      
         B     EDITEND                                                          
         SPACE 1                                                                
* SID VALIDATION - SID=(STATION)                                                
         SPACE 1                                                                
EDOPT32  CLC   12(4,R4),=C'SID '   SID OPTIONS                                  
         BNE   EDOPT36                                                          
         L     RE,ASPOOLD                                                       
         LA    RE,PPROF1-SPOOLD(RE)                                             
         CLI   0(RE),C'N'                                                       
         BE    EDOPT34                                                          
         MVI   SIDOPT,C'Y'                                                      
         CLI   1(R4),0             CAN BE FOLLOWED BY STATION                   
         BE    EDITEND                                                          
         MVC   WORK(4),=4X'F0'                                                  
         MVC   DUB(4),22(R4)                                                    
         MVI   DUB+4,C'T'                                                       
         GOTO1 MSPACK,DMCB,WORK,DUB,WORK+4                                      
         MVC   SIDSTAT,WORK+6                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
* NEWSID VALIDATION - SID=(SCHEME)                                              
         SPACE 1                                                                
EDOPT34  MVI   SIDOPT,C'Y'                                                      
         MVI   DPOPT,C'Y'          FORCE DPT OPTION                             
         MVC   NSIDSCHM,=C'ALL '                                                
         CLI   1(R4),0                                                          
         BE    EDOPT34A                                                         
         CLI   1(R4),2                                                          
         BL    SCANERR                                                          
         CLI   1(R4),3                                                          
         BH    SCANERR                                                          
         MVC   NSIDSCHM,22(R4)                                                  
         SPACE 1                                                                
* MAKE SURE SCHEME EXISTS                                                       
         SPACE 1                                                                
EDOPT34A XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         CLC   =C'ALL',NSIDSCHM                                                 
         BE    EDOPT34B                                                         
         GOTO1 CLPACK,DMCB,NSIDSCHM,KEY+2                                       
EDOPT34B GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SCANERR                                                          
         MVC   NSIDDSKA,KEY+14     SAVE DISK ADDRESS                            
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT36  CLC   12(3,R4),=C'PER'    PERIOD SETTING                               
         BNE   EDOPT40                                                          
         L     RE,ASPOOLD                                                       
         LA    RE,PPROF1-SPOOLD(RE)                                             
         CLI   0(RE),C'N'          TEST NEWSID                                  
         BE    EDOPT38                                                          
         SPACE 1                                                                
         CLI   1(R4),1                                                          
         BNE   SCANERR                                                          
         SPACE 1                                                                
         MVC   SIDPER,22(R4)                                                    
         CLI   SIDPER,C'1'                                                      
         BL    SCANERR                                                          
         CLI   SIDPER,C'4'                                                      
         BH    SCANERR                                                          
         B     EDITEND                                                          
         SPACE 1                                                                
* NEWSID VALIDATION - PER=PER(/YR)                                              
         SPACE 1                                                                
EDOPT38  MVI   ERROR,NOSCHYET                                                   
         OC    NSIDDSKA,NSIDDSKA   MUST ENTER SCHEME FIRST                      
         BZ    EDTERR                                                           
*                                                                               
         LA    R0,5                                                             
         LA    R1,22(R4)           POINT TO PERIOD                              
*                                                                               
EDOPT38A CLI   0(R1),C' '                                                       
         BE    EDOPT38B                                                         
         CLI   0(R1),C'/'                                                       
         BE    EDOPT38B                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,EDOPT38A                                                      
         B     SCANERR                                                          
*                                                                               
EDOPT38B LA    RE,5                                                             
         SR    RE,R0               GIVES NUM CHARS PRESENT FOR PERIOD           
         ST    RE,FULL                                                          
         CH    RE,=H'4'                                                         
         BH    SCANERR                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NSIDPER(0),22(R4)                                                
         OC    NSIDPER,=CL4'    '                                               
*                                                                               
         CLI   0(R1),C'/'          TEST YEAR PRESENT                            
         BNE   EDOPT38K                                                         
*                                                                               
         ZIC   R0,1(R4)                                                         
         S     R0,FULL                                                          
         BCTR  R0,0                ADJUST FOR /                                 
         LA    R1,1(R1)            POINT TO FIRST DIGIT OF YEAR                 
         ST    R1,FULL             SAVE ADDRESS                                 
         CH    R0,=H'2'                                                         
         BNE   SCANERR                                                          
*                                                                               
EDOPT38C CLI   0(R1),C'0'                                                       
         BL    SCANERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    SCANERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,EDOPT38C                                                      
         L     RE,FULL                                                          
         PACK  DUB,0(2,RE)                                                      
         CVB   R0,DUB                                                           
         STC   R0,NSIDYEAR                                                      
         SPACE 1                                                                
* VALIDATE PERIOD NAME *                                                        
         SPACE 1                                                                
EDOPT38K XC    KEY,KEY                                                          
         MVC   KEY+14(4),NSIDDSKA                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,EPNCODEQ     FIND PERIOD NAME ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   SCANERR                                                          
*                                                                               
         USING EPNELEM,R6                                                       
         ZIC   R0,EPNLEN                                                        
         SH    R0,=H'2'                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1                                                            
         LA    R1,EPNDATA                                                       
*                                                                               
EDOPT38M CLC   NSIDPER,1(R1)                                                    
         BE    EDITEND                                                          
         LA    R1,5(R1)                                                         
         BCT   R0,EDOPT38M                                                      
         B     SCANERR                                                          
         SPACE 1                                                                
EDOPT40  CLC   12(3,R4),=C'ST '     STATION (NEWSID)                            
         BNE   EDOPT42                                                          
         MVC   WORK(4),=4X'F0'                                                  
         MVC   DUB(4),22(R4)                                                    
         MVI   DUB+4,C'T'                                                       
         GOTO1 MSPACK,DMCB,WORK,DUB,WORK+4                                      
         MVC   SIDSTAT,WORK+6                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT42  CLC   12(5,R4),=C'STACK'  STACK OPTIONS                                
         BNE   EDOPT44                                                          
         MVI   STACKOPT,C'Y'                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT44  CLC   12(5,R4),=C'SURVEY' SURVEY OPTION                                
         BNE   EDOPT46                                                          
         MVC   SURVOPT,22(R4)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT46  CLC   12(4,R4),=C'OVER'   DEMO OVERRIDE OPTION                         
         BNE   EDOPT48                                                          
         MVC   OVEROPT,22(R4)                                                   
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT48  CLC   12(8,R4),=C'COMPRESS'                                            
         BNE   EDOPT50                                                          
         MVI   COMPOPT,C'Y'                                                     
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT50  CLC   12(5,R4),=C'TRACE'                                               
         BNE   EDOPT52                                                          
         MVI   TRACEOPT,C'Y'                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT52  LA    R1,PUTOPT                                                        
         CLC   12(3,R4),=C'PUT'                                                 
         BE    EDOPT54                                                          
         LA    R1,RTGOPT                                                        
         CLC   12(3,R4),=C'RTG'                                                 
         BE    EDOPT54                                                          
         CLC   12(3,R4),=C'SHR'                                                 
         BNE   EDOPT56                                                          
         SPACE 1                                                                
EDOPT54  MVI   0(R1),C'N'                                                       
         CLC   8(4,R4),=F'1'                                                    
         BE    EDITEND                                                          
         MVI   0(R1),C'Y'                                                       
         CLC   8(4,R4),=F'2'                                                    
         BE    EDITEND                                                          
         B     SCANERR                                                          
         SPACE 1                                                                
EDOPT56  DS    0H                                                               
         CLC   12(3,R4),=C'FMT'                                                 
         BNE   EDOPT58                                                          
         CLC   22(2,R4),=C'EB'                                                  
         BNE   EDOPT58                                                          
         MVI   EBOPT,C'Y'                                                       
         MVI   DPOPT,C'Y'          FORCE DAYPART OPTION                         
         OC    UPOPT,UPOPT         TEST UPGRADE EXPRESSION ENTERED              
         BNZ   EDITEND             YES - DO NOT USE SID UPGRADES                
         MVC   UPOPT(3),=C'SID'                                                 
         MVI   UPFILE,C'T'         DEFAULT FILE IS TP                           
         MVI   SIDOPT,C'Y'                                                      
         MVI   SIDUPOPT,C'Y'                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT58  CLC   12(4,R4),=C'DOWN'   DOWNLOAD OPTION                              
         BNE   EDOPT60                                                          
         MVI   DOWNOPT,C'D'                                                     
         B     EDITEND                                                          
         SPACE 1                                                                
EDOPT60  DS    0H                                                               
         B     SCANERR                                                          
         SPACE 1                                                                
EDITEND  LA    R4,42(R4)                                                        
         BCT   R5,EDOPT2                                                        
         B     YES                                                              
         EJECT                                                                  
* EDIT FILTERS                                                                  
         SPACE 1                                                                
EDITFILT NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    YES                                                              
         MVI   SCANLEN,32          SET TABLE ENTRY LENGTH                       
         BAS   RE,CLRBLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LA    R5,FILTERS                                                       
         LTR   R0,R0                                                            
         BZ    SCANERR                                                          
         SPACE 1                                                                
FILT2    DS    0H                                                               
         SPACE 1                                                                
FILT6    CLC   12(4,R4),=C'PROGTYP'                                             
         BE    FILT6A                                                           
         CLC   12(3,R4),=C'PRG'                                                 
         BE    FILT6A                                                           
         CLC   12(3,R4),=C'PT '                                                 
         BNE   FILT8                                                            
FILT6A   LA    RE,PTLIST                                                        
         B     FILT9                                                            
         SPACE 1                                                                
FILT8    CLC   12(7,R4),=C'DAYPART'                                             
         BE    FILT8A                                                           
         CLC   12(3,R4),=C'DPT'                                                 
         BE    FILT8A                                                           
         CLC   12(3,R4),=C'DP '                                                 
         BNE   FILT10                                                           
FILT8A   LA    RE,DPLIST                                                        
         SPACE 1                                                                
FILT9    MVI   ERROR,NEEDSID                                                    
         CLI   SIDOPT,C'Y'         TEST SID SPECIFIED                           
         BE    FILT9A                                                           
         GOTO1 VCURSERR                                                         
*                                                                               
FILT9A   CLI   1(R4),8             MAX 8 ENTRIES                                
         BH    SCANERR                                                          
         MVC   0(8,RE),22(R4)      MOVE INPUT TO LIST                           
         LA    RF,8                                                             
*                                                                               
FILT9B   CLI   0(RE),C' '                                                       
         BNH   FILTEND                                                          
         CLI   0(RE),C'A'                                                       
         BL    SCANERR                                                          
         CLI   0(RE),C'Z'                                                       
         BH    SCANERR                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,FILT9B                                                        
         B     FILTEND                                                          
         SPACE 1                                                                
FILT10   CLC   12(3,R4),=C'TOP'    SHOW ONLY TOP NN                             
         BNE   FILT12                                                           
         OC    8(4,R4),8(R4)                                                    
         BZ    SCANERR                                                          
         MVC   TOPOPT,8(R4)                                                     
         B     FILTEND                                                          
         SPACE 1                                                                
FILT12   CLC   12(3,R4),=C'MAX'    DONT SHOW ABOVE MAX                          
         BNE   FILT14                                                           
         BAS   RE,VALCASH                                                       
         BNE   SCANERR                                                          
         MVC   MAXOPT,WORK                                                      
         LA    R1,MAXCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     FILTEND                                                          
         SPACE 1                                                                
FILT14   CLC   12(3,R4),=C'MIN'    DONT SHOW BELOW MIN                          
         BNE   FILT16                                                           
         BAS   RE,VALCASH                                                       
         BNE   SCANERR                                                          
         MVC   MINOPT,WORK                                                      
         LA    R1,MINCOL                                                        
         BAS   RE,ANYCOL                                                        
         B     FILTEND                                                          
         SPACE 1                                                                
FILT16   DS    0H                                                               
         SPACE 1                                                                
FILT20   CLC   12(3,R4),=C'EFF'    EFFECTIVE DATE FILTER                        
         BNE   FILT22                                                           
         CLI   11(R4),0            SECOND OPERAND S/B 1-3                       
         BE    SCANERR                                                          
         CLI   11(R4),3                                                         
         BH    SCANERR                                                          
         MVC   EFFOPT,11(R4)                                                    
         B     EDITEND                                                          
         SPACE 1                                                                
FILT22   DS    0H                                                               
         B     SCANERR                                                          
         SPACE 1                                                                
FILTEND  LA    R4,32(R4)                                                        
         BCT   R0,FILT2                                                         
         B     YES                                                              
         EJECT                                                                  
* ODDMENTS                                                                      
         SPACE 1                                                                
ANYCOL   MVI   0(R1),1             DEFAULT COLUMN IS 1                          
         CLI   0(R4),4                                                          
         BNER  RE                                                               
         MVC   0(1,R1),15(R4)      MOVE IN SPECIFIED COLUMN                     
         NI    0(R1),X'0F'                                                      
         CLI   0(R1),1                                                          
         BL    NO                                                               
         CLI   0(R1),8                                                          
         BH    NO                                                               
         BR    RE                                                               
         SPACE 1                                                                
VALCASH  NTR1                                                                   
         ZIC   R0,1(R4)                                                         
         GOTO1 CASHVAL,DMCB,22(R4),(R0)                                         
         CLI   DMCB,X'FF'                                                       
         BE    NO                                                               
         MVC   WORK(4),DMCB+4                                                   
         B     YES                                                              
         SPACE 1                                                                
EXIT     XIT1                                                                   
         SPACE 1                                                                
EDTERR   GOTO1 VGETERR             NO RETURN HERE                               
         DC    H'0'                                                             
         SPACE 1                                                                
SCANERR  MVI   ERROR,INVALID       SET ERROR MESSAGE NUMBER                     
         GOTO1 VCURSERR            * NO RETURN HERE                             
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
CLRBLOCK LA    R1,BLOCK            CLEAR BLOCK FOR SCANNER                      
         LA    R0,10                                                            
         XC    0(48,R1),0(R1)                                                   
         LA    R1,48(R1)                                                        
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
* EDIT DETAILS                                                                  
         SPACE 1                                                                
VALIDET  NTR1                                                                   
         GOTO1 ANY                                                              
         MVI   SCANLEN,32          SET TABLE ENTRY LENGTH                       
         BAS   RE,CLRBLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R5,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R5,R5                                                            
         BZ    SCANERR                                                          
         STC   R5,NDETS                                                         
         LA    R6,DETS                                                          
         SR    R7,R7                                                            
         SPACE 1                                                                
VDET2    BAS   RE,CHEKLIST                                                      
         BNE   SCANERR                                                          
         MVC   0(1,R6),12(R4)      SAVE FIRST LETTER                            
         MVC   1(3,R6),WORK        AND NUMBER/WIDTH/AVE CONTROL                 
*                                                                               
         CLI   WORK,DTLEQMUL       TEST MULTI-RANK                              
         BNE   *+12                                                             
         MVI   WDEMS,13                                                         
         MVI   MULTIOPT,C'Y'                                                    
*                                                                               
         CLI   WORK,DTLEQMGR       TEST MKTGRP                                  
         BNE   VDET4                                                            
         BAS   RE,VALMGR                                                        
         MVI   4(R6),C'+'          FORCE IN MGRMKT SPEC                         
         MVC   5(3,R6),DETMGMKT+8                                               
         LA    R6,4(R6)            BUMP TO NEXT DETAIL                          
         ZIC   RE,NDETS                                                         
         LA    RE,1(RE)            BUMP DETAIL COUNTER                          
         STC   RE,NDETS                                                         
         B     VDET4X                                                           
*                                                                               
VDET4    ZIC   RE,WORK+1                                                        
         LA    R7,1(RE,R7)         ADD WIDTH+1                                  
*                                                                               
VDET4X   LA    R4,32(R4)                                                        
         LA    R6,4(R6)                                                         
         BCT   R5,VDET2                                                         
*                                                                               
         STC   R7,WDETS                                                         
*                                                                               
         CLI   EBOPT,C'Y'          TEST EB FORMAT                               
         BNE   VDETX                                                            
         SPACE 1                                                                
* ADD ADDITIONAL EB DETAILS USER COULDN'T SPECIFY BUT ALWAYS THERE *            
         SPACE 1                                                                
         ST    R6,AEBDTLX          SET LAST INPUT DETAIL ADDRESS                
         MVI   0(R6),C'B'          BOOK                                         
         MVC   1(3,R6),DETBOOK+8                                                
         LA    R6,4(R6)                                                         
         MVI   0(R6),C'P'          PROGRAM                                      
         MVC   1(3,R6),DETPROG+8                                                
         LA    R6,4(R6)                                                         
         MVI   0(R6),C'C'          COMMENT                                      
         MVC   1(3,R6),DETCOMM+8                                                
*                                                                               
         MVI   COMPOPT,C'Y'        FORCE COMPRESS OPTION                        
         MVI   AVOPT,C'Y'          FORCE AVAIL OPTION                           
*                                                                               
         ZIC   RE,NDETS                                                         
         LA    RE,3(RE)                                                         
         STC   RE,NDETS                                                         
*                                                                               
         LA    R6,DETS                                                          
         CLI   0(R6),DTLEQMGR      TEST MKTGRP                                  
         BNE   *+8                                                              
         LA    R6,4(R6)                                                         
         LA    R0,4(R6)            GET FIRST BLOCKED DETAIL ADDRESS             
         ST    R0,AEBDTL            AND SAVE IT                                 
         ZIC   R7,2(R6)            WIDTH OF FIRST COLUMN                        
         LA    R7,1(R7)            ALLOW FOR BOXCOL                             
         ST    R7,EBDISP           GIVES EB DETAIL DISPLACEMENT                 
         LA    R7,31(R7)           DETS ARE ALWAYS 30 WIDE + 2 BOXCOLS          
         STC   R7,WDETS                                                         
VDETX    B     YES                                                              
         EJECT                                                                  
* SUBROUTINE TO VALIDATE MARKET GROUP INPUT *                                   
         SPACE 1                                                                
VALMGR   NTR1                                                                   
*                                                                               
         LA    R0,DETS             MUST BE FIRST DETAIL                         
         CR    R6,R0                                                            
         BNE   SCANERR                                                          
*                                                                               
         CLI   1(R4),0                                                          
         BNH   SCANERR                                                          
         CLI   1(R4),5                                                          
         BH    SCANERR                                                          
*                                                                               
         CLI   22(R4),C'G'                                                      
         BL    SCANERR                                                          
         CLI   22(R4),C'Z'                                                      
         BH    SCANERR                                                          
*                                                                               
         LA    R1,23(R4)           POINT TO FIRST CHAR OF MKTGRP                
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BZ    VALMGRX                                                          
*                                                                               
VALMGR2  CLI   0(R1),C'*'                                                       
         BE    VALMGRX                                                          
         CLI   0(R1),C'0'                                                       
         BL    SCANERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    SCANERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALMGR2                                                       
*                                                                               
VALMGRX  MVI   MGROPT,C'Y'         SET OPTION ON                                
         MVI   NSTATS,1                                                         
         MVC   STATS(5),22(R4)     SET MKTGRP IN STATION LIST                   
         B     YES                                                              
         EJECT                                                                  
* ROUTINE TO CHECK AGAINST LIST                                                 
         SPACE 1                                                                
CHEKLIST NTR1                                                                   
         ZIC   R5,0(R4)                                                         
         LTR   R5,R5                                                            
         BZ    NO                                                               
         BCTR  R5,0                                                             
         LA    R1,DETLIST                                                       
         SPACE 1                                                                
CL2      EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),0(R1)                                                   
         BE    CL4                                                              
         LA    R1,L'DETLIST(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   CL2                                                              
         B     NO                                                               
         SPACE 1                                                                
CL4      MVC   WORK(4),8(R1)                                                    
         CLI   EBOPT,C'Y'                                                       
         BNE   YES                                                              
         TM    12(R1),X'80'        TEST INVALID FOR EB OPTION                   
         BZ    YES                                                              
         B     NO                                                               
         EJECT                                                                  
* DETAILS WITH X'80' IN LAST BYTE ARE INVALID FOR EB OPTION                     
         SPACE 1                                                                
DETLIST  DS    0XL13                                                            
         DC    C'STATION ',AL1(DTLEQSTA,10,00,00),X'00'                         
DETBOOK  DC    C'BOOK    ',AL1(DTLEQBK,06,00,00),X'80'                          
         DC    C'WEEKS   ',AL1(DTLEQWKS,05,00,00),X'80'                         
         DC    C'MONTH   ',AL1(DTLEQMON,05,00,00),X'80'                         
         DC    C'QUARTER ',AL1(DTLEQQTR,07,00,00),X'80'                         
         DC    C'YEAR    ',AL1(DTLEQYR,04,00,00),X'80'                          
DETPROG  DC    C'PROGRAM ',AL1(DTLEQPRG,16,00,00),X'80'                         
         DC    C'DAY     ',AL1(DTLEQDAY,07,00,00),X'00'                         
         DC    C'TIME    ',AL1(DTLEQTIM,11,00,00),X'00'                         
         DC    C'RANK    ',AL1(DTLEQRNK,05,00,00),X'80'                         
         DC    C'FILTER  ',AL1(DTLEQFLT,06,00,00),X'80'                         
         DC    C'DATE    ',AL1(DTLEQDAT,05,00,00),X'80'                         
         DC    C'LENGTH  ',AL1(DTLEQLEN,06,00,00),X'80'                         
         DC    C'DAYPART ',AL1(DTLEQDPT,07,00,00),X'00'                         
         DC    C'DP      ',AL1(DTLEQDPT,07,00,00),X'00'                         
         DC    C'PROGTYP ',AL1(DTLEQPT,07,00,00),X'00'                          
         DC    C'PT      ',AL1(DTLEQPT,07,00,00),X'00'                          
         DC    C'PERIOD  ',AL1(DTLEQPER,06,00,00),X'80'                         
         DC    C'BP      ',AL1(DTLEQPER,06,00,00),X'80'                         
         DC    C'MARKET  ',AL1(DTLEQMKT,16,00,00),X'80'                         
         DC    C'UPGRADE ',AL1(DTLEQUPG,16,00,00),X'80'                         
         DC    C'EFFECT  ',AL1(DTLEQEFF,06,00,00),X'80'                         
         DC    C'MULTI   ',AL1(DTLEQMUL,00,00,00),X'00'                         
         DC    C'MRANK   ',AL1(DTLEQMUL,00,00,00),X'00'                         
         DC    C'MKTGRP  ',AL1(DTLEQMGR,00,00,00),X'00'                         
         DC    C'MGROUP  ',AL1(DTLEQMGR,00,00,00),X'00'                         
DETCOMM  DC    C'COMMENT ',AL1(DTLEQCOM,30,00,00),X'80'                         
DETMGMKT DC    C'.MGRMKT ',AL1(DTLEQMKM,00,00,00),X'00'                         
         DC    X'FF'                                                            
         EJECT                                                                  
* EDIT AVERAGES                                                                 
         SPACE 1                                                                
VALIAVE  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    YES                                                              
         MVI   SCANLEN,32          SET TABLE ENTRY LENGTH                       
         BAS   RE,CLRBLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R5,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R5,R5                                                            
         BZ    SCANERR                                                          
         SPACE 1                                                                
VAVE2    CLI   12(R4),C'R'         R AVERAGE MEANS REPORT                       
         BNE   VAVE4                                                            
         MVC   REPAVE,22(R4)                                                    
         B     VAVE10                                                           
         SPACE 1                                                                
VAVE4    BAS   RE,CHEKLIST                                                      
         BNE   SCANERR                                                          
         LA    R1,DETS             OTHERWISE AVERAGE REFERS TO                  
         ZIC   R0,NDETS            PREVIOUSLY SPECIFIED DETAIL                  
         BCT   R0,VAVE6            BUT NOT MOST DETAILED LEVEL                  
         B     SCANERR                                                          
         SPACE 1                                                                
VAVE6    CLC   1(1,R1),WORK                                                     
         BE    VAVE8                                                            
         CLI   0(R1),C'R'          CANT HAVE AVERAGES OR TOTALS                 
         BE    SCANERR             BELOW RANK                                   
         LA    R1,4(R1)                                                         
         BCT   R0,VAVE6                                                         
         B     SCANERR                                                          
         SPACE 1                                                                
VAVE8    MVC   3(1,R1),22(R4)                                                   
         CLI   3(R1),C' '                                                       
         BH    *+8                                                              
         MVI   3(R1),C'A'          DEFAULT IS AVERAGE                           
         CLI   3(R1),C'A'          ALLOW A T OR B                               
         BE    VAVE10                                                           
         CLI   3(R1),C'T'                                                       
         BE    VAVE10                                                           
         CLI   3(R1),C'B'                                                       
         BE    VAVE10                                                           
         B     SCANERR                                                          
         SPACE 1                                                                
VAVE10   LA    R4,32(R4)                                                        
         BCT   R5,VAVE2                                                         
         B     YES                                                              
         SPACE 1                                                                
NO       MVI   ERROR,INVALID                                                    
         LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXIT                                                             
*                                                                               
RELO     DS    A                                                                
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD5D                                                       
         PRINT ON                                                               
       ++INCLUDE SPRES95WRK                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSIR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPRES75   05/01/02'                                      
         END                                                                    

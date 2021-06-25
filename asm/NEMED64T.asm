*          DATA SET NEMED64T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED64    AT LEVEL 050 AS OF 05/15/86                      
*PHASE T31E64A,+0                                                               
         TITLE 'T31E64 - NETWORK PACKAGE EVALUATION'                            
**** MEMORY USAGE:                                                              
*     R7 -> ANETSW2 -> NDDEMBLK                                                 
*                      DBLOCK                                                   
*                      WORKING STORAGE                                          
*  ANETSW1 IS USED TO STORE NETBLOCK, SO IT CAN BE RESTORED FOR EACH            
*    REPORT TYPE.                                                               
*                                                                               
T31E64   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTPK**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         USING T31E64+4096,RA                                                   
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS2                                                       
         USING PEPRD,R7                                                         
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         EJECT                                                                  
*              CONTROL OF REPORT                                                
*                                                                               
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
*                                                                               
***** INITIALIZE CONTROLS                                                       
*                                                                               
*                                                                               
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'W'        USE NEXT PER FOR WEEKS                       
         MVI   PERTYPE+1,1         USE MONTHS IF TOO BIG                        
         MVI   PERTYPE+2,0         NEVER USE QUARTERS                           
*                                                                               
         MVI   NBDATA,C'P'         SELECT PACKAGE RECORDS                       
         MVI   NBESTOPT,C'Y'       TO GET ESTIMATED DEMOS                       
*                             ****** PROCESS DATES AND PACKAGE RECORD           
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     EXIT LOOP AFTER DATES PROCESSED              
         BE    IN6                                                              
         B     PROCDAT             CONTINUE FOR OTHER MODES                     
*                                                                               
IN6      LA    R1,MAXMONTS                                                      
         ST    R1,NUMMONS          MAX NUMBER OF MONTHS IN LIST                 
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE   AND FILL LIST             
         MVI   NBDATA,C'U'         AND SELECT UNIT RECORDS ONLY                 
*                                                                               
         L     RF,ANETWS1          SAVE IMPORTANT PART OF NETBLOCK              
         LA    R1,NBESTUN-NETBLOCK   AT W/S AREA 1                              
         MOVE  ((RF),(R1)),NETBLOCK   TO ALLOW MULTIPLE REPORTS                 
*                                                                               
         MVI   RCSUBPRG,1                                                       
         CLI   SPLTHRE,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,LOOP             3-DEMO EVALUATION                            
         MVI   RCSUBPRG,2                                                       
         CLI   SPLMLTI,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,LOOP             MULTI-DEMO EVALUATION                        
         CLI   SPLMLTI,C'F'                                                     
         BNE   *+8                                                              
         BAS   RE,LOOP             MULTI-DEMO EVALUATION                        
         CLI   SPLMLTI,C'S'                                                     
         BNE   *+8                                                              
         BAS   RE,LOOP             MULTI-DEMO EVALUATION                        
         MVI   RCSUBPRG,3                                                       
         CLI   SPLWEEK,C'Y'                                                     
         BNE   XIT                                                              
         BAS   RE,LOOP             WEEKLY GRID                                  
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
LOOP     NTR1                                                                   
*                                                                               
         L     RE,ANETWS1          RESTORE SAVED PART OF NETBLOCK               
         LA    R1,NBESTUN-NETBLOCK   FROM W/S AREA 1                            
         MOVE  (NETBLOCK,(R1)),(RE)   TO ALLOW MULTIPLE REPORTS                 
*                                                                               
         MVC   PAGE,=H'1'          INITIALIZE                                   
         MVI   PAKFLAG,0                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   BUYACTIV,C'N'                                                    
         XCEF  BUYACCUM,1040                                                    
*                                                                               
****** NOW DO SPECIALIZED CHANGES DEPENDING ON REPORT TYPE                      
         CLI   RCSUBPRG,3                                                       
         BNE   LP2                                                              
         MVI   NBSEQ,C'Q'          READ IN OLD PROG ORDER FOR WEEKLY            
*                                                                               
LP2      MVI   NBHUNOPT,C'Y'                                                    
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF FIRST UNIT RECORD                         
         BE    CKPROG                                                           
         CLI   NBMODE,NBREQLST     IF NO MORE UNITS                             
         BE    LASTONE                                                          
         B     GETUNIT             OTHER MODES ARE IGNORED                      
*                                                                               
*                                                                               
CKPROG   TM    NBSUBMSK,NBSBMPRG   IF NEW PROGRAM                               
         BZ    CKDATE                                                           
         BAS   RE,BUYTOTS             PRINT TOTALS FOR OLD PROGRAM              
         MVI   BUYACTIV,C'N'          RESET FOR NEXT                            
         MVC   SAVEPROG,NBPROGNM      SAVE NEW PROGRAM INFO                     
         MVC   SAVELEN,NBLEN                                                    
         LA    R6,MONLIST             START AT FIRST DATE SET AGAIN             
         LA    R5,1                                                             
*                                                                               
CKDATE   CLC   NBACTDAT,2(R6)      FIND MONTH SET FOR NEW PROGRAM               
         BNH   GOTMON                                                           
NEWMONTH LA    R6,4(R6)            POINT TO NEXT DATE-SET                       
         LA    R5,1(R5)                                                         
         B     CKDATE                                                           
*                                                                               
GOTMON   STC   R5,CURMONTH                                                      
         BAS   RE,POST             FILL IN UNIT LINE.                           
         B     GETUNIT                                                          
*                                                                               
LASTONE  BAS   RE,BUYTOTS          FINISH OLD PROGRAM                           
         MVI   PAKFLAG,1           TELLS FORMAT WE ARE IN PAKTOTS               
*                                   (NEEDED TO RUN OLD CODE)                    
         BAS   RE,PAKTOTS                                                       
         XIT1                                                                   
*                                                                               
PROCERR  DC    F'0'                                                             
*                                                                               
         EJECT                                                                  
*              ROUTINES TO POST                                                 
         SPACE 2                                                                
POST     NTR1                                                                   
         LA    R6,BUYACCUM                                                      
         USING ACCUMD,R6                                                        
         LH    R1,NBESTUN          UNITS                                        
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   BUYACTIV,C'Y'                                                    
         A     R1,ACUN                                                          
         ST    R1,ACUN                                                          
         LH    R1,NBESTHUT                                                      
         A     R1,ACHUT                                                         
         ST    R1,ACHUT                                                         
         LH    R1,NBESTSHR         SHARE                                        
         A     R1,ACSHR                                                         
         ST    R1,ACSHR                                                         
         L     R1,NBACTUAL         CASH                                         
         M     R0,=F'1'            CONVERT TO DOLLARS                           
         D     R0,=F'100'                                                       
         A     R1,ACDOL                                                         
         ST    R1,ACDOL                                                         
*                                                                               
         LH    R1,NBESTHOM+2       HOME POINTS                                  
         A     R1,ACHOMRTG                                                      
         ST    R1,ACHOMRTG                                                      
         L     R1,NBESTHOM+4       HOME IMPS                                    
         A     R1,ACHOMIMP                                                      
         ST    R1,ACHOMIMP                                                      
*                                                                               
         LA    R2,NDESTDEM         PROCESS ALL DEMOS                            
         ZIC   R3,NDNDEMOS                                                      
         LTR   R3,R3                                                            
         BZ    POST4               SKIP LOOP IF NO DEMOS                        
         LA    R4,ACDEMVPH                                                      
*                                                                               
POST2    LH    R1,0(R2)            VPH                                          
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         LH    R1,2(R2)            POINTS                                       
         A     R1,4(R4)                                                         
         ST    R1,4(R4)                                                         
         L     R1,4(R2)            IMPS                                         
         A     R1,8(R4)                                                         
         ST    R1,8(R4)                                                         
         LA    R2,8(R2)                                                         
         LA    R4,12(R4)                                                        
         BCT   R3,POST2                                                         
*                                                                               
POST4    L     R1,NBACTUAL         CONVERT NBACTUAL TO DOLLARS                  
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,ACTCSTDL         SAVE IT FOR LATER                            
         ZIC   R1,CURMONTH        DISPLACE INTO WEEK ACCUMS                     
         BCTR  R1,0                                                             
         MH    R1,=H'20'                                                        
         LA    R6,BUYWKACC(R1)                                                  
         USING WEEKD,R6                                                         
         LM    R1,R5,ACWEKHOM                                                   
         AH    R1,NBESTHOM+2       HOMES GRP                                    
         AH    R2,NDESTDEM+2       DEMO  GRP                                    
         AH    R3,NBESTUN                                                       
         AH    R4,NBESTHUT                                                      
         A     R5,ACTCSTDL                                                      
         STM   R1,R5,ACWEKHOM                                                   
         B     XIT                                                              
         EJECT                                                                  
*              BUY TOTALS                                                       
         SPACE 3                                                                
BUYTOTS  NTR1                                                                   
         CLI   BUYACTIV,C'Y'                                                    
         BNE   XIT                                                              
         MVC   P+4(16),SAVEPROG                                                 
         CLI   RCSUBPRG,3                                                       
         BE    BUYTOTS4                                                         
         EDIT  (1,SAVELEN),(3,P+20)                                             
         LA    R6,BUYACCUM                                                      
         USING ACCUMD,R6                                                        
         L     R1,ACHUT            AVERAGE HUT                                  
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,P+25)                                                    
         MVI   P+27,C' '                                                        
         L     R1,ACSHR            SHARE                                        
         SR    R0,R0                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'5'                                                         
         EDIT  (R1),(3,P+29)                                                    
         MVI   P+31,C' '                                                        
         L     R0,ACHOMRTG         AVE RTG                                      
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,P+33),1                                                  
         CLI   RCSUBPRG,1                                                       
         BNE   BUYTOTS2                                                         
         BAS   RE,FORM3                                                         
         LA    R4,50                                                            
         BAS   RE,ROLL                                                          
         B     XIT                                                              
         SPACE 2                                                                
BUYTOTS2 BAS   RE,FORMMULT                                                      
         LA    R4,50                                                            
         BAS   RE,ROLL                                                          
         B     XIT                                                              
         SPACE 2                                                                
BUYTOTS4 LA    R6,BUYWKACC                                                      
         BAS   RE,FORMWEEK                                                      
         LA    R4,130                                                           
         BAS   RE,ROLL                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AND CLEAR ACCUMULATORS                            
         SPACE 3                                                                
ROLL     NTR1                                                                   
         LA    R2,BUYACCUM                                                      
         LA    R3,PAKACCUM                                                      
         SR    R0,R0                                                            
         SPACE 2                                                                
ROLL2    L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R0,0(R2)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ROLL2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PACKAGE TOTALS                                                   
         SPACE 3                                                                
PAKTOTS  NTR1                                                                   
         MVC   P+4(14),=C'PACKAGE TOTALS'                                       
         CLI   RCSUBPRG,3                                                       
         BE    PAKTOTS4                                                         
         LA    R6,PAKACCUM                                                      
         MVC   P+21(6),=C'COST=$'                                               
         EDIT  (4,NBPAKCST),(8,P+27),ALIGN=LEFT                                 
         CLI   RCSUBPRG,2                                                       
         BE    PAKTOTS2                                                         
         MVC   P2+26(7),=C'CPM/CPP'                                             
         BAS   RE,FORM3                                                         
         B     XIT                                                              
         SPACE 2                                                                
PAKTOTS2 BAS   RE,FORMMULT                                                      
         B     XIT                                                              
         SPACE 2                                                                
PAKTOTS4 LA    R6,PAKWKACC                                                      
         BAS   RE,FORMWEEK                                                      
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT FOR 3-DEMO EVALUATION                                     
         SPACE 3                                                                
FORM3    NTR1                                                                   
         XC    TOTDISP,TOTDISP                                                  
         MVC   CPMDISP,=F'132'                                                  
         MVI   AVEDISP,X'FF'                                                    
         CLI   PAKFLAG,1           SKIP WHEN IN PKG TOTALS                      
         BE    FORM3D                                                           
         MVI   CPMDISP,X'FF'                                                    
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         CLI   SPLCPMH+5,0                                                      
         BE    FORM3D                                                           
         LA    R2,SPLCPM                                                        
         ZIC   R3,SPLCPMH+5                                                     
         DROP  R5                                                               
         SR    R4,R4                                                            
         MVI   TOTDISP,X'FF'                                                    
         SPACE 2                                                                
FORM3B   CLI   0(R2),C'1'                                                       
         BNE   *+8                                                              
         ST    R4,TOTDISP                                                       
         CLI   0(R2),C'2'                                                       
         BNE   *+8                                                              
         ST    R4,AVEDISP                                                       
         CLI   0(R2),C'3'                                                       
         BNE   *+8                                                              
         ST    R4,CPMDISP                                                       
         LA    R2,1(R2)                                                         
         LA    R4,132(R4)                                                       
         BCT   R3,FORM3B                                                        
         SPACE 2                                                                
         USING ACCUMD,R6                                                        
FORM3D   OC    ACUN,ACUN                                                        
         BZ    XIT                                                              
         EDIT  (4,ACUN),(4,P+37)   UNITS                                        
         LA    R2,ACHOMVPH                                                      
         LA    R3,P+43                                                          
         ZIC   R4,NDNDEMOS                                                      
         LTR   R4,R4                                                            
         BZ    XITFORM                                                          
         CH    R4,=H'3'                                                         
         BL    *+8                                                              
         LA    R4,3                                                             
         LA    R4,1(R4)                                                         
         SPACE 2                                                                
FORM32   CLI   PAKFLAG,1             VPH                                        
         BE    FORM34              FOR NON PACKAGE TOTALS                       
         OC    0(4,R2),0(R2)                                                    
         BZ    FORM34                                                           
         L     R0,0(R2)                                                         
         SRDA  R0,31                                                            
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BCTR  R3,0                                                             
         EDIT  (R1),(4,0(R3))                                                   
         LA    R3,1(R3)                                                         
         MVI   2(R3),C' '                                                       
         SPACE 2                                                                
FORM34   OC    8(4,R2),8(R2)       IMPS                                         
         BZ    FORM36                                                           
         CLI   TOTDISP,X'FF'                                                    
         BE    FORM35                                                           
         A     R3,TOTDISP                                                       
         NETGO NVPRDEM,DMCB,(C'I',0),8(R2),2(R3)                                
         S     R3,TOTDISP                                                       
         SPACE 2                                                                
FORM35   CLI   AVEDISP,X'FF'                                                    
         BE    FORM36                                                           
         L     R0,8(R2)                                                         
         SRDA  R0,31                                                            
         D     R0,ACUN             IMPRESSIONS/UNIT                             
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CLI   NBSURVEY,C'N'       IF MAJOR NETWORK                             
         BNE   FORM35B                                                          
         LA    R1,50(R1)           ROUND TO NEAREST 10000                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         M     R0,=F'100'                                                       
         SPACE 1                                                                
FORM35B  ST    R1,WORK                                                          
         A     R3,AVEDISP                                                       
         NETGO NVPRDEM,DMCB,(C'I',0),WORK,2(R3)                                 
         S     R3,AVEDISP                                                       
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
FORM40   L     R1,NBPAKCST         SHOW CPP/CPM                                 
         CLI   PAKFLAG,1             ON PACKAGE TOTALS                          
         BE    FORM42                                                           
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
XITFORM  MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT FOR MULTI-DEMO EVALUATION                                 
         SPACE 3                                                                
FORMMULT NTR1                                                                   
         USING ACCUMD,R6                                                        
         EDIT  (4,ACUN),(4,P+37)   UNITS                                        
         MVI   PLINES,C' '                                                      
         MOVE  (PLINES+1,999),PLINES                                            
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         LA    R2,SPLDATA                                                       
         ZIC   R3,SPLDATAH+5                                                    
         DROP  R5                                                               
         CLI   PAKFLAG,1           SKIP IF PACKAGE TOTALS                       
         BE    FM2                                                              
         LA    R2,FIXDATA                                                       
         LA    R3,5                                                             
         B     FM2                                                              
         SPACE 2                                                                
FIXDATA  DC    C'12345'                                                         
         SPACE 2                                                                
FM2      LR    R0,R3                                                            
         CLI   NDNDEMOS,8          IF MORE THAN 7 DEMOS                         
         BL    *+8                                                              
         SLL   R3,1                TWICE AS MANY LINES                          
         STC   R3,ALLOWLIN                                                      
         LR    R3,R0                                                            
         LA    R4,PLINES                                                        
         SPACE 2                                                                
FM4      IC    RF,0(R2)            C'1' - C'5'                                  
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         BCTR  RF,0                0-4                                          
         SLL   RF,2                0-16                                         
         BAS   RE,FM10                                                          
         LA    R2,1(R2)                                                         
         LA    R4,200(R4)                                                       
         BCT   R3,FM4                                                           
         LR    R3,R0                                                            
         LA    R4,PLINES                                                        
         SPACE 2                                                                
FM6      MVC   P+43(67),43(R4)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+54(56),110(R4)                                                 
         CLC   P,SPACES                                                         
         BE    FM8                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
FM8      LA    R4,200(R4)                                                       
         BCT   R3,FM6                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SPECIFIC FORMATTING FOR MULTI-DEMO - CONTROL                     
         SPACE 3                                                                
FM10     NTR1                                                                   
         ST    RF,SAVERF                                                        
         LA    R1,MULTLIST(RF)    RF=0/4/8/12/16                                
         MVC   43(4,R4),0(R1)                                                   
         LA    R2,ACHOMVPH                                                      
         LA    R4,46(R4)                                                        
         ZIC   R3,NDNDEMOS                                                      
         LTR   R3,R3                                                            
         BZ    XITFM               IF NO DEMOS                                  
         LA    R3,1(R3)                                                         
         SPACE 2                                                                
FM12     L     RF,SAVERF                                                        
         B     FM14(RF)                                                         
         SPACE 2                                                                
FM14     B     FM20                                                             
         B     FM22                                                             
         B     FM24                                                             
         B     FM28                                                             
         B     FM30                                                             
         SPACE 2                                                                
MULTLIST DC    C'VPH '                                                          
         DC    C'IMP '                                                          
         DC    C'CPM '                                                          
         DC    C'GRP '                                                          
         DC    C'CPP '                                                          
         SPACE 2                                                                
FM16     LA    R2,12(R2)                                                        
         LA    R4,8(R4)                                                         
         BCT   R3,FM12                                                          
XITFM    B     XIT                                                              
         EJECT                                                                  
*              MULTI-DEMO  VPH/IMP/CPM/GRP/CPP                                  
         SPACE 3                                                                
FM20     L     R1,0(R2)            VPH                                          
         LA    R1,5(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'2'                                                         
         OC    ACUN,ACUN                                                        
         BZ    FM16                                                             
         D     R0,ACUN                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    FM16                                                             
         EDIT  (R1),(7,1(R4))                                                   
         B     FM16                                                             
         SPACE 2                                                                
FM22     NETGO NVPRDEM,DMCB,(C'I',0),8(R2),1(R4)                                
         B     FM16                                                             
         SPACE 2                                                                
FM24     L     R1,NBPAKCST         CPM                                          
         CLI   PAKFLAG,1           SKIP IF PACKAGE TOTALS                       
         BE    FM26                                                             
         L     R1,ACDOL                                                         
         SPACE 2                                                                
FM26     LTR   R1,R1                                                            
         BZ    FM16                                                             
         OC    8(4,R2),8(R2)                                                    
         BZ    FM16                                                             
         M     R0,=F'2000'                                                      
         D     R0,8(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,1(R4)),2,FLOAT=$                                         
         B     FM16                                                             
         SPACE 2                                                                
FM28     L     R1,4(R2)            GRP                                          
         LTR   R1,R1                                                            
         BZ    FM16                                                             
         EDIT  (R1),(7,1(R4)),1                                                 
         B     FM16                                                             
         SPACE 2                                                                
FM30     L     R1,NBPAKCST         CPP                                          
         CLI   PAKFLAG,1           SKIP IF PACKAGE TOTALS                       
         BE    FM32                                                             
         L     R1,ACDOL                                                         
         SPACE 2                                                                
FM32     LTR   R1,R1                                                            
         BZ    FM16                                                             
         OC    4(4,R2),4(R2)                                                    
         BZ    FM16                                                             
         M     R0,=F'20'                                                        
         D     R0,4(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(7,1(R4)),FLOAT=$                                           
         B     FM16                                                             
         EJECT                                                                  
*              FORMATTING WEEKLY EVALUATION                                     
         SPACE 3                                                                
FORMWEEK NTR1                                                                   
         SH    R6,=H'200'          BACK UP FOR UNITS                            
         USING ACCUMD,R6                                                        
         EDIT  (4,ACUN),(4,P+20)                                                
         LA    R6,200(R6)                                                       
         USING WEEKD,R6                                                         
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         LA    R2,SPLWKDT                                                       
         LA    R3,P+25                                                          
         ZIC   R4,SPLWKDTH+5                                                    
         DROP  R5                                                               
         CH    R4,=H'4'                                                         
         BL    *+8                                                              
         LA    R4,4                                                             
         SPACE 2                                                                
FW2      BAS   RE,FW4                                                           
         LA    R2,1(R2)                                                         
         LA    R3,132(R3)                                                       
         BCT   R4,FW2                                                           
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
FW4      NTR1                                                                   
         L     R4,NUMMONS          CONTROL WEEKS                                
         SPACE 2                                                                
FW6      OC    ACWEKUN,ACWEKUN                                                  
         BZ    *+8                                                              
         BAS   RE,FW8                                                           
         LA    R3,6(R3)                                                         
         LA    R6,20(R6)                                                        
         BCT   R4,FW6                                                           
         B     XIT                                                              
         SPACE 2                                                                
FW8      NTR1                                                                   
         ZIC   RF,0(R2)            BRANCH TO SELECTED ROUTINE                   
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     FW10(RF)                                                         
         SPACE 2                                                                
FW10     B     FW12                HUT                                          
         B     FW14                DEMO GRPS                                    
         B     FW20                UNITS                                        
         B     FW18                HOME GRPS                                    
         B     FW16                COST                                         
         SPACE 2                                                                
FW12     L     R0,ACWEKHUT                                                      
         SRDA  R0,31                                                            
         D     R0,ACWEKUN                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(6,(R3)),1                                                  
         B     XIT                                                              
         SPACE 2                                                                
FW14     EDIT  (4,ACWEKDEM),(6,(R3)),1                                          
         B     XIT                                                              
         SPACE 2                                                                
FW16     EDIT  (4,ACWEKDOL),(6,(R3))                                            
         B     XIT                                                              
         SPACE 2                                                                
FW18     EDIT  (4,ACWEKHOM),(6,(R3)),1                                          
         B     XIT                                                              
         SPACE 2                                                                
FW20     EDIT  (4,ACWEKUN),(6,(R3))                                             
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         MVC   H5+86(4),SPLNET                                                  
         MVC   H6+76(36),SPLPAKN                                                
         OC    NBPAKCPM,NBPAKCPM   DISPLAY GUARANTEED CPM                       
         BZ    HOOKB                                                            
         MVC   H7+76(15),=C'GUARANTEED CPM '                                    
         EDIT  (4,NBPAKCPM),(9,H7+91),2,FLOAT=$,ALIGN=LEFT                      
         SPACE 1                                                                
HOOKB    CLI   RCSUBPRG,2                                                       
         BE    HOOK10                                                           
         BH    HOOK20                                                           
         DROP  R5                                                               
         EJECT                                                                  
*              3-DEMO EVALUATION ROUTINES                                       
         SPACE 3                                                                
         CLI   AVEDISP,X'FF'                                                    
         BE    *+10                                                             
         MVC   H6+45(16),=C'(AVERAGE OPTION)'                                   
         MVC   H9+047(11),=C'---HOMES---'                                       
         MVC   H10+47(11),=C'IMPS.  GRPS'                                       
         CLI   CPMDISP,X'FF'                                                    
         BE    *+10                                                             
         MVC   H11+46(11),=C' CPM    CPP'                                       
         SR    R2,R2               COUNTER FOR NTH DEMO                         
         ZIC   R3,NDNDEMOS                                                      
         LTR   R3,R3                                                            
         BZ    XITHOOK             IF NO DEMOS                                  
         CH    R3,=H'3'                                                         
         BL    *+8                                                              
         LA    R3,3                                                             
         LA    R4,H9+60                                                         
*                                                                               
*                                  GET NAME OF NTH DEMO                         
HOOK2    NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   4(7,R4),WORK                                                     
*                                                                               
         LR    R5,R4                                                            
         LA    R6,15                                                            
         SPACE 2                                                                
HOOK4    CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         BCT   R6,HOOK4                                                         
         MVC   132(15,R4),=C'VPH  IMPS  GRPS'                                   
         CLI   CPMDISP,X'FF'                                                    
         BE    *+10                                                             
         MVC   264(15,R4),=C'      CPM   CPP'                                   
         LA    R2,1(R2)                                                         
         LA    R4,17(R4)                                                        
         BCT   R3,HOOK2                                                         
XITHOOK  B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES FOR MULTI-DEMO                                 
         SPACE 3                                                                
HOOK10   MVC   H9+49(5),=C'HOMES'                                               
         MVC   H10+49(5),=C'-----'                                              
         SR    R2,R2               COUNTER FOR NTH DEMO                         
         ZIC   R3,NDNDEMOS                                                      
         LTR   R3,R3                                                            
         BZ    XITHKM                                                           
         LA    R4,H9+55                                                         
         LA    R5,1                                                             
*                                                                               
*                                  GET NAME OF NTH DEMO                         
HOOK12   NETGO NVDEMCON,DMCB,((R2),NDDEMBLK),DBLOCK,(7,WORK)                    
         MVC   0(7,R4),WORK                                                     
*                                                                               
         CH    R5,=H'7'                                                         
         BH    HOOK14                                                           
         GOTO1 UNDERLIN,DMCB,(7,(R4)),132(R4)                                   
         SPACE 2                                                                
HOOK14   LA    R2,1(R2)                                                         
         LA    R4,8(R4)                                                         
         CH    R5,=H'7'                                                         
         BNE   *+8                                                              
         LA    R4,H10+55                                                        
         LA    R5,1(R5)                                                         
         BCT   R3,HOOK12                                                        
XITHKM   B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR WEEKLY EVALUATION                                   
         SPACE 3                                                                
HOOK20   MVC   H9+020(5),=C'UNITS'                                              
         MVC   H10+20(5),=C'-----'                                              
         CLI   PERTYPE,C'M'                                                     
         BNE   *+10                                                             
         MVC   H1+48(7),=C'MONTHLY'                                             
         LA    R2,MONLIST                                                       
         L     R3,NUMMONS                                                       
         LA    R4,H9+26                                                         
         SPACE 2                                                                
HOOK22   GOTO1 DATCON,DMCB,(2,(R2)),(8,WORK)                                    
         MVC   0(5,R4),WORK                                                     
         CLI   PERTYPE,C'M'       SHOW YEAR ON MONTHLY 'WEEKLIES'               
         BNE   *+10                                                             
         MVC   3(2,R4),WORK+6                                                   
         MVC   132(5,R4),=C'-----'                                              
         CLI   PERTYPE,C'M'                                                     
         BNE   HOOK23                                                           
         CLI   NBUSER+2,C'B'       MNTH TYPE (B OR C)N                          
         BNE   HOOK23                                                           
         GOTO1 DATCON,DMCB,(2,(R2)),(4,0(R4))                                   
         GOTO1 DATCON,DMCB,(2,2(R2)),(4,132(R4))                                
         SPACE 2                                                                
HOOK23   LA    R2,4(R2)                                                         
         LA    R4,6(R4)                                                         
         BCT   R3,HOOK22                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(5),=C'DATA='                                                
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         LA    R2,SPLWKDT                                                       
         LA    R3,WORK+5                                                        
         ZIC   R4,SPLWKDTH+5                                                    
         DROP  R5                                                               
         SPACE 2                                                                
HOOK24   ZIC   R1,0(R2)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'6'                                                         
         LA    R1,TYPLIST(R1)                                                   
         MVC   0(6,R3),0(R1)                                                    
         CLC   0(6,R3),=C'TARGET'                                               
         BNE   HOOK26                                                           
*                                                                               
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(7,WORK)                       
         MVC   7(5,R3),SPACES                                                   
*                                                                               
HOOK26   LA    R2,1(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,HOOK24                                                        
         GOTO1 SQUASHER,DMCB,WORK,60                                            
         MVC   H6+36(34),WORK                                                   
         GOTO1 CENTER,DMCB,H6+36,34                                             
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
         EJECT                                                                  
TYPLIST  DC    C'HUT   '                                                        
         DC    C'TARGET'                                                        
         DC    C'UNITS '                                                        
         DC    C'HOMES '                                                        
         DC    C'COST  '                                                        
         EJECT                                                                  
*              COMMON WITH EDIT                                                 
*        SPACE 3                                                                
PEPRD    DSECT                                                                  
*              NETDEMOD HERE                                                    
*              DEDBLOCK                                                         
         PRINT OFF                                                              
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
*                                  LOCAL WORKING STORAGE                        
SAVEPROG DS    CL16                SAVE OLD PROGRAM NAME                        
SAVELEN  DS    CL1                 SAVE OLD PROGRAM LENGTH                      
ACTCSTDL DS    F                   NBACTUAL CONVERTED TO DOLLARS                
PAKFLAG  DS    CL1                 SET WHEN WE ARE IN PAKTOTS                   
*                                                                               
MAXMONTS EQU   16                  MAXIMUM NUMBER OF MONTHS                     
PERTYPE  DS    CL3                 1ST BYTE TELLS WEEK OR MONTH                 
MONLIST  DS    CL(4*MAXMONTS)      MONTHLIST                                    
NUMMONS  DS    F                   NUM OF MONS IN LIST                          
CURMONTH DS    CL1                 CURRENT MONTH NUMBER                         
*                                                                               
BUYACCUM DS    50F                                                              
BUYWKACC DS    80F                                                              
PAKACCUM DS    50F                                                              
PAKWKACC DS    80F                                                              
PLINES   DS    5CL200                                                           
BUYACTIV DS    CL1                                                              
TOTDISP  DS    F                                                                
AVEDISP  DS    F                                                                
CPMDISP  DS    F                                                                
SAVERF   DS    A                                                                
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
WEEKD    DSECT                                                                  
ACWEKHOM DS    F                                                                
ACWEKDEM DS    F                                                                
ACWEKUN  DS    F                                                                
ACWEKHUT DS    F                                                                
ACWEKDOL DS    F                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE4D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED64T  05/01/02'                                      
         END                                                                    

*          DATA SET NEWRI2BT   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI2B    AT LEVEL 037 AS OF 06/10/86                      
*PHASE T3202BA,+0                                                               
         TITLE 'T3202B - P4 (GE NETWORK ALLOCATION)'                            
         PRINT NOGEN                                                            
T3202B   CSECT                                                                  
         NMOD1 0,**P4PR**                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T3202B+4096,R9                                                   
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
         L     R1,=A(DPBUFF)       ADDRESS DAYPART BUFFER                       
         A     R1,RELO                                                          
         ST    R1,ADPBUFF                                                       
         SPACE 1                                                                
         L     R1,=A(PLBUFF)       AND PRINT LINE BUFFER                        
         A     R1,RELO                                                          
         ST    R1,APLBUFF                                                       
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 3                                                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         MVI   ANYSORT,C'N'                                                     
         BAS   RE,SETGROUP         SET UP GROUPS IF NEEDED                      
         SPACE 1                                                                
         MVI   NBDATA,C'U'         READ UNIT RECORDS                            
         OI    NBSPLOPT,X'80'      TURN ON SPLIT OPTION                         
         MVI   NBHUNOPT,C'Y'       WE CAN DEAL IN HUNDREDS                      
         MVI   NBRESUME,NBPROCPK   RESUME READING PACKAGES                      
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBACTOPT,C'Y'                                                    
         MVC   NBSELUOP,SCHOPT     SELECT ESTIMATED OR ACTUAL                   
         CLI   PFBOPT,C'Y'         PFB DEMO OPTION                              
         BNE   *+8                                                              
         MVI   NBESTOPT,C'P'                                                    
         SPACE 1                                                                
INIT2    XC    DETDISP,DETDISP     FIGURE DISPACEMENT FOR MAIN REPORTS          
         CLI   LEFTOPT,C'Y'                                                     
         BE    LOOP                                                             
         MVI   DETDISP+3,2                                                      
         MVC   BWIDTH,=H'33'                                                    
         CLI   NLENS,2                                                          
         BH    LOOP                                                             
         MVC   BWIDTH,=H'29'                                                    
         MVI   DETDISP+3,8                                                      
         BE    LOOP                                                             
         MVC   BWIDTH,=H'25'                                                    
         MVI   DETDISP+3,17                                                     
         EJECT                                                                  
*              MAIN NETIO LOOP                                                  
         SPACE 3                                                                
LOOP     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BE    GOTUN                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    LASTONE                                                          
         B     LOOP                                                             
         SPACE 1                                                                
GOTDATE  XC    PERTYPE,PERTYPE     SET UP WEEK LIST                             
         MVI   PERTYPE,C'M'                                                     
         XC    MLIST,MLIST                                                      
         NETGO NVWKLST,DMCB,=F'12',MLIST,PERTYPE                                
         LA    R2,MLIST                                                         
         LA    R3,1                                                             
         SPACE 1                                                                
GOTDATE2 STC   R3,NMONTHS          SET NUMBER OF MONTHS                         
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R2),0                                                          
         BNE   GOTDATE2                                                         
         B     LOOP                                                             
         SPACE 1                                                                
GOTUN    BAS   RE,UNIT                                                          
         B     LOOP                                                             
         SPACE 1                                                                
LASTONE  BAS   RE,REPORTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              PROCESS UNIT RECORDS                                             
         SPACE 3                                                                
UNIT     NTR1                                                                   
         OC    NBACTUN,NBACTUN     UNIT MUST BE ACTIVE                          
         BZ    XIT                                                              
         XC    SORTAREA,SORTAREA   FILL SORT AREA WITH UNIT DETAILS             
         BAS   RE,GETGROUP                                                      
         BAS   RE,GETPROD                                                       
         MVC   SDP,NBDPNAM                                                      
         SPACE 1                                                                
         LA    R2,MLIST            LOOK UP MONTH NUMBER                         
         LA    R3,1                                                             
         ZIC   R0,NMONTHS                                                       
         SPACE 1                                                                
UNIT2    STC   R3,SMONTH                                                        
         CLC   NBACTDAT,2(R2)      (CHECK V END DATE)                           
         BNH   UNIT4                                                            
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,UNIT2                                                         
         B     XIT                                                              
         SPACE 1                                                                
UNIT4    BAS   RE,GETACCUM                                                      
         BAS   RE,PUTSORT          THEN WE'RE DONE                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FOR SORT PUTS AND TRACE                                  
         SPACE 3                                                                
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         MVI   ANYSORT,C'Y'                                                     
         MVC   SDP,=8X'FF'         ANOTHER RECORD FOR TOTAL DAYPARTS            
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         SPACE 1                                                                
         OC    SGRP,SGRP           IF GROUPS ARE INVOLVED                       
         BZ    PUTSORT4                                                         
         MVC   SDP,NBDPNAM         RECORDS FOR GROUP TOTALS                     
         MVC   SPRD,=X'FFFFFF'                                                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         MVC   SDP,=8X'FF'                                                      
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         SPACE 1                                                                
PUTSORT4 MVC   SDP,NBDPNAM         RECORDS FOR REPORT TOTALS                    
         MVC   SGRP,=X'FFFFFF'                                                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         MVC   SDP,=8X'FF'                                                      
         GOTO1 SORTER,DMCB,=C'PUT',SORTAREA                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP PRODUCT GROUP                                  
         SPACE 3                                                                
SETGROUP NTR1                                                                   
         MVC   CODEDISP,=H'10'     PRESET DISPLACEMENTS                         
         MVC   NAMEDISP,=H'17'                                                  
         CLI   SPLPRO+1,C'='                                                    
         BNE   SGXIT                                                            
         NETGO NVSETSPT,DMCB                                                    
         USING PRDHDR,R4                                                        
         LA    R4,KEY                                                           
         L     R2,=A(GROUPLST)                                                  
         A     R2,RELO                                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM                                                 
         SPACE 1                                                                
SG2      LA    R4,KEY                                                           
         AI    PKEYPRD+2,1         SKIP TO NEXT PRODUCT                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SG4                                                              
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   0(1,R2),PCODE+1     PRODUCT CODE                                 
         MVC   1(3,R2),PGRP1       FIND MATCH OF PRODUCT GROUP SCHEME           
         CLC   SPLPRO(1),1(R2)     MAY BE FIRST                                 
         BE    SG3                                                              
         MVC   1(3,R2),PGRP2       OR SECOND                                    
         CLC   SPLPRO(1),1(R2)                                                  
         BE    SG3                                                              
         MVC   1(3,R2),PGRP3       OR THIRD                                     
         CLC   SPLPRO(1),1(R2)                                                  
         BE    SG3                                                              
         XC    1(3,R2),1(R2)                                                    
         SPACE 1                                                                
SG3      LA    R2,4(R2)                                                         
         B     SG2                                                              
         SPACE 1                                                                
SG4      MVI   0(R2),X'FF'                                                      
         LA    R4,KEY              GET SCHEME RECORD                            
         XC    KEY,KEY                                                          
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID,SPLPRO                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 READ                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R2,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING PRGEL01,R2                                                       
         MVC   BREAK,PRGBK1        DIG OUT BREAK NAME AND LENGTH                
         OC    BREAK,SPACES                                                     
         CLC   BREAK+9(3),SPACES   ALLOW FOR BIG BREAK NAME                     
         BE    *+16                                                             
         MVC   CODEDISP,=H'13'                                                  
         MVC   NAMEDISP,=H'20'                                                  
         MVC   BRLEN,PRGBK1LN                                                   
         MVC   BREAK2,SPACES                                                    
         MVI   BRLEN2,0                                                         
         CLI   PRGBK2LN,0                                                       
         BE    SGXIT                                                            
         MVC   BREAK2,PRGBK2                                                    
         OC    BREAK2,SPACES                                                    
         CLC   BREAK2+9(3),SPACES                                               
         BE    *+16                                                             
         MVC   CODEDISP,=H'13'                                                  
         MVC   NAMEDISP,=H'20'                                                  
         MVC   BRLEN2,BRLEN                                                     
         AC    BRLEN2,PRGBK2LN                                                  
         SPACE 1                                                                
SGXIT    XC    FILENAME,FILENAME   RESET TO UNT FILE                            
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ESTABLISH PRODUCT GROUP FOR UNIT                      
         SPACE 3                                                                
GETGROUP NTR1                                                                   
         CLI   SPLPRO+1,C'='                                                    
         BNE   YES                                                              
         CLI   NBSPLPRN,X'FF'       UNALLOCATED MISSES                          
         BE    NO                                                               
         CLI   NBSPLPRN,0                                                       
         BE    NO                                                               
         L     R2,=A(GROUPLST)                                                  
         A     R2,RELO                                                          
         SPACE 1                                                                
FG2      CLC   NBSPLPRN,0(R2)                                                   
         BE    FG6                                                              
         SPACE 1                                                                
FG4      LA    R2,4(R2)                                                         
         B     FG2                                                              
         SPACE 1                                                                
FG6      MVC   SGRP,1(R2)          GROUP ESTABLISHED                            
         CLC   SPLPRO+2(3),=C'ALL'                                              
         BE    YES                                                              
         UNPK  WORK(5),2(3,R2)                                                  
         LA    R2,WORK                                                          
         LA    R1,SPLPRO+2                                                      
         LA    R0,3                                                             
         SPACE 1                                                                
FG8      CLI   0(R1),C'A'          FILTER SPECIFIC GROUP                        
         BL    FG10                                                             
         CLC   0(1,R1),0(R2)                                                    
         BNE   NO                                                               
         SPACE 1                                                                
FG10     LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,FG8                                                           
         B     YES                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              GET PRODUCT CODE                                                 
         SPACE 3                                                                
GETPROD  NTR1                                                                   
         L     R2,ANETWS1                                                       
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         LA    R0,220                                                           
         DROP  R2                                                               
         SPACE 1                                                                
GETPROD2 CLC   NBSPLPRN,3(R2)                                                   
         BE    GETPROD4                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,GETPROD2                                                      
         B     XIT                                                              
         SPACE 1                                                                
GETPROD4 MVC   SPRD,0(R2)                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL IN ACCUMULATORS                                  
         SPACE 3                                                                
GETACCUM NTR1                                                                   
         LA    R2,SACCUMS                                                       
         USING ACCUMD,R2                                                        
         LA    R3,ACUN1+3                                                       
         LA    R4,LENS                                                          
         ZIC   R0,NLENS                                                         
         SPACE 1                                                                
GAC2     CLC   NBLEN,0(R4)         LOOK FOR LENGTH IN ANALYSIS LIST             
         BE    GAC4                                                             
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,GAC2                                                          
         B     GAC6                                                             
         SPACE 1                                                                
GAC4     MVI   0(R3),1             FOUND SO PUT A UNIT INTO LIST                
         SPACE 1                                                                
*                                  RATING POINTS                                
GAC6     MVC   ACGRP+2(2),NDACTDEM+2                                            
         L     R1,NBCALCOS                                                      
         M     R0,=F'1'                                                         
         D     R0,=F'100'                                                       
         ST    R1,ACDOL            DOLLARS                                      
         DROP  R2                                                               
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
REPORTS  NTR1                                                                   
         CLI   ANYSORT,C'Y'                                                     
         BNE   XIT                                                              
         XC    LASTREC,LASTREC                                                  
         MVI   DPCOUNT,0                                                        
         B     REPNEXT2                                                         
         SPACE 1                                                                
REPNEXT  MVC   LASTREC,SORTAREA                                                 
         SPACE 1                                                                
REPNEXT2 GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    REPEOF                                                           
         OC    LASTREC,LASTREC                                                  
         BZ    REPDET                                                           
         CLC   LASTREC(3),0(R2)    CHECK CONTROL BREAKS                         
         BNE   GROUPCB                                                          
         CLC   LASTREC(6),0(R2)                                                 
         BNE   BRANDCB                                                          
         CLC   LASTREC(14),0(R2)                                                
         BNE   DPTCB                                                            
         B     REPDET                                                           
         SPACE 1                                                                
GROUPCB  BAS   RE,DPFORM                                                        
         BAS   RE,CHUNK                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     REPDET                                                           
         SPACE 1                                                                
BRANDCB  BAS   RE,DPFORM                                                        
         BAS   RE,CHUNK                                                         
         B     REPDET                                                           
         SPACE 1                                                                
DPTCB    BAS   RE,DPFORM                                                        
         CLI   DPCOUNT,3                                                        
         BL    REPDET                                                           
         BAS   RE,CHUNK                                                         
         SPACE 1                                                                
REPDET   MVC   SORTAREA,0(R2)                                                   
         BAS   RE,POSTUNIT                                                      
         B     REPNEXT                                                          
         SPACE 1                                                                
REPEOF   BAS   RE,DPFORM           END OF SORT FILE                             
         BAS   RE,CHUNK                                                         
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST UNIT DETAILS                                     
         SPACE 3                                                                
POSTUNIT NTR1                                                                   
         LA    R2,SACCUMS                                                       
         L     R3,ADPBUFF          ADD TO FIRST LINE OF BUFFER                  
         BAS   RE,ADDEM            WHICH WE ARE USING FOR TOTALS                
         ZIC   R1,SMONTH           THEN PICK UP MONTH NUMBER                    
         MH    R1,=H'20'                                                        
         AR    R3,R1               AND ADDRESS SPECIFIC MONTH LINE              
         BAS   RE,ADDEM            AND ADD INTO THIS TOO                        
         B     XIT                                                              
         SPACE 1                                                                
ADDEM    NTR1                      ROUTINE ADDS 5 ACCUMS FROM LINE              
         LA    R0,5                ADDRESSED BY R2 TO R3 LINE                   
         SPACE 1                                                                
ADDEM2   L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADDEM2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT A DAYPART BLOCK                                
         SPACE 3                                                                
*              INPUTS              ADPBUFF=A(DAYPART BUFFER)                    
*                                  APLBUFF=A(PRINT LINE BUFFER)                 
         SPACE 1                                                                
DPFORM   NTR1                                                                   
         AI    DPCOUNT,1                                                        
         L     R3,APLBUFF                                                       
         A     R3,DETDISP                                                       
         CLI   DPCOUNT,1           IF THIS IS THE FIRST DAYPART                 
         BNE   *+8                                                              
         BAS   RE,PRODMON          FORMAT PRODUCT AND MONTHS                    
         LA    R3,28(R3)           SPACE PAST PRODUCT AND MONTH                 
         ZIC   R1,DPCOUNT                                                       
         BCTR  R1,0                                                             
         MH    R1,BWIDTH                                                        
         AR    R3,R1               ADDRESS CORRECT DAYPART BLOCK                
         BAS   RE,DPFLOAT          FLOAT DAYPART NAME ABOVE BLOCK               
         LA    R3,264(R3)          START EDIT ON LINE 3                         
         L     R2,ADPBUFF                                                       
         LA    R2,20(R2)                                                        
         ZIC   R0,NMONTHS                                                       
         SPACE 1                                                                
DPFORM2  BAS   RE,FORMAT           EDIT EACH MONTH                              
         LA    R2,20(R2)                                                        
         LA    R3,132(R3)                                                       
         BCT   R0,DPFORM2                                                       
         LA    R3,132(R3)                                                       
         L     R2,ADPBUFF                                                       
         BAS   RE,FORMAT           AND THEN THE TOTALS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT PRODUCT AND MONTHS                             
         SPACE 3                                                                
*              INPUT               R3=A(OUTPUT AREA)                            
         SPACE 1                                                                
PRODMON  NTR1                                                                   
         LA    R3,265(R3)          COME DOWN TO LINE 3                          
         MVC   0(13,R3),=C'REPORT TOTALS'                                       
         CLI   SGRP,X'FF'                                                       
         BE    PRODMON2                                                         
         MVC   0(13,R3),=C'GROUP TOTALS  '                                      
         CLI   SPRD,X'FF'                                                       
         BE    PRODMON2                                                         
         BAS   RE,GETPNAM                                                       
         MVC   0(20,R3),SAVEPRDN                                                
         SPACE 1                                                                
PRODMON2 LA    R3,21(R3)                                                        
         LA    R2,MLIST                                                         
         ZIC   R0,NMONTHS                                                       
         SPACE 1                                                                
PRODMON4 GOTO1 DATCON,DMCB,(2,2(R2)),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         ZIC   R4,DMCB             TO THE FOLLOWING SUNDAY                      
         LA    R5,7                                                             
         SR    R5,R4                                                            
         BZ    PRODMON6                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R5)                                      
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
PRODMON6 PACK  WORK+8(8),WORK+2(2)                                              
         CVB   R1,WORK+8                                                        
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS-3(R1)                                                  
         MVC   0(3,R3),0(R1)                                                    
         MVC   3(2,R3),WORK                                                     
         LA    R2,4(R2)                                                         
         LA    R3,132(R3)                                                       
         BCT   R0,PRODMON4                                                      
         SPACE 1                                                                
         LA    R3,132(R3)                                                       
         MVC   0(5,R3),=C'TOTAL'                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FLOAT DAYPART NAME OVER BLOCK                         
         SPACE 3                                                                
*              INPUT               R3=A(OUTPUT AREA)                            
*                                  BWIDTH=H'WIDTH OF BLOCK'                     
         SPACE 1                                                                
DPFLOAT  NTR1                                                                   
         LH    R4,BWIDTH                                                        
         BCTR  R4,0                                                             
         MVC   0(5,R3),=C'TOTAL'  CENTER 'TOTAL'                                
         CLI   SDP,X'FF'                                                        
         BE    *+10                                                             
         MVC   0(8,R3),SDP        OR DAYPART NAME                               
         GOTO1 CENTER,DMCB,(R3),(R4)                                            
         CLI   BOXOPT,C'Y'                                                      
         BE    XIT                                                              
         SPACE 1                                                                
DPFLOAT2 CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R3),C'-'          THEN FILL WITH DASHES                        
         LA    R3,1(R3)                                                         
         BCT   R4,DPFLOAT2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF DAYPART BLOCKS                    
         SPACE 3                                                                
*                                  APLBUFF=A(PRINT LINE BUFFER)                 
         SPACE 1                                                                
CHUNK    NTR1                                                                   
         CLI   FORCEHED,C'Y'       ARE WE SKIPPING PAGE                         
         BE    CHUNK2                                                           
         ZIC   R4,NMONTHS          WE NEED ROOM FOR N MONTHS                    
         LA    R4,6(R4)            PLUS 6 OTHER LINES                           
         ZIC   R1,LINE                                                          
         AR    R1,R4                                                            
         CH    R1,=H'58'                                                        
         BNH   CHUNK4                                                           
         MVI   FORCEHED,C'Y'                                                    
         SPACE 1                                                                
CHUNK2   GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
CHUNK4   L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         MVI   2(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         L     R2,APLBUFF                                                       
         ZIC   R4,NMONTHS                                                       
         LA    R4,4(R4)                                                         
         SPACE 1                                                                
CHUNK6   MVC   P(132),0(R2)                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   0(132,R2),SPACES                                                 
         LA    R2,132(R2)                                                       
         BCT   R4,CHUNK6                                                        
         SPACE 1                                                                
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   BOXROWS,SPACES                                                   
         MVI   DPCOUNT,0                                                        
         DROP  R5                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL FORMATTING OF TOTALS                          
         SPACE 3                                                                
*              INPUTS              R2=A(ACCUMS)                                 
*                                  R3=A(OUTPUT AREA)                            
*                                  NLENS=NUMBER OF LENGTHS ANALYSED             
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         USING ACCUMD,R2                                                        
         LA    R4,ACUN1                                                         
         ZIC   R5,NLENS                                                         
         SPACE 1                                                                
FORMAT2  EDIT  (4,0(R4)),(4,0(R3)) EDIT UNITS BY SPOT-LENGTH                    
         LA    R3,5(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,FORMAT2                                                       
         SPACE 1                                                                
         L     R1,ACGRP            THEN GRPS (ROUTINE BELOW)                    
         BCTR  R3,0                                                             
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         BAS   RE,GRPED                                                         
         LA    R3,8(R3)                                                         
         EDIT  (4,ACDOL),(10,0(R3)) AND, FINALLY COST                           
         SPACE 1                                                                
*                                  CLEAR OUT THIS ENTRY                         
         XC    ACENTRY(L'ACENTRY),ACENTRY                                       
         DROP  R2                                                               
         B     XIT                                                              
         SPACE 1                                                                
GRPED    NTR1                      FORMAT GRPS                                  
         EDIT  (R1),(8,0(R3)),1    (1 DECIMAL IF POSSIBLE)                      
         CLI   0(R3),C' '                                                       
         BE    XIT                                                              
         EDIT  (R1),(9,0(R3))                                                   
         MVI   8(R3),C' '                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         NETGO NVTITOUT,DMCB,H1+46                                              
         SPACE 1                                                                
         LA    R3,H4                                                            
         LA    R1,132(R3)                                                       
         ST    R1,ATHISH                                                        
         MVC   0(6,R3),=C'CLIENT'                                               
         LR    R5,R3                                                            
         AH    R3,CODEDISP                                                      
         AH    R5,NAMEDISP                                                      
         MVC   0(3,R3),SPLCLI      USE CODE AND NAME FROM SCREEN                
         MVC   0(20,R5),SPLCLIN                                                 
         BAS   RE,HOOKGRP                                                       
         BAS   RE,HOOKPRD                                                       
         BAS   RE,HOOKEST                                                       
         BAS   RE,HOOKNET                                                       
         BAS   RE,HOOKDPT                                                       
         BAS   RE,HOOKPACK                                                      
         BAS   RE,HOOKBOX                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINES - PRODUCT GROUP(S) AND NAME                            
         SPACE 3                                                                
HOOKGRP  NTR1                                                                   
         CLI   SGRP,0                                                           
         BE    XIT                                                              
         CLI   SGRP,X'FF'                                                       
         BE    XIT                                                              
         CLC   SGRP,SAVEGRP                                                     
         BE    HOOKGRP6                                                         
         MVC   SAVEGRP,SGRP                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID(3),SGRP                                                   
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         LA    R2,PRGEL                                                         
         SPACE 1                                                                
HOOKGRP2 CLI   0(R2),X'10'                                                      
         BE    HOOKGRP4                                                         
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     HOOKGRP2                                                         
         SPACE 1                                                                
         USING PRGEL10,R2                                                       
HOOKGRP4 MVC   SAVEGRPN,PRGNAM1                                                 
         MVC   SAVEG2N,PRGNAM2                                                  
         SPACE 1                                                                
HOOKGRP6 L     R3,ATHISH                                                        
         LR    R5,R3                                                            
         MVC   0(12,R3),BREAK      FIRST LEVEL BREAK/CODE/NAME                  
         AH    R3,CODEDISP                                                      
         AH    R5,NAMEDISP                                                      
         MVC   0(1,R3),SGRP                                                     
         UNPK  WORK(5),SGRP+1(3)                                                
         ZIC   R1,BRLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),WORK                                                     
         MVC   0(24,R5),SAVEGRPN                                                
         CLI   BRLEN2,0                                                         
         BE    HOOKGRP8                                                         
         SPACE 1                                                                
         L     R3,ATHISH                                                        
         LA    R3,132(R3)                                                       
         ST    R3,ATHISH                                                        
         MVC   0(12,R3),BREAK2     SECOND LEVEL BREAK/CODE/NAME                 
         LR    R5,R3                                                            
         AH    R3,CODEDISP                                                      
         AH    R5,NAMEDISP                                                      
         MVC   0(1,R3),SGRP                                                     
         UNPK  WORK(5),SGRP+1(3)                                                
         ZIC   R1,BRLEN2                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),WORK                                                     
         MVC   0(24,R5),SAVEG2N                                                 
         SPACE 1                                                                
HOOKGRP8 XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         SPACE 1                                                                
HOOKALLX L     R3,ATHISH                                                        
         LA    R3,132(R3)                                                       
         ST    R3,ATHISH                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINES - PRODUCT CODE AND NAME                                
         SPACE 3                                                                
HOOKPRD  NTR1                                                                   
         L     R3,ATHISH                                                        
         CLI   SPLPRO+1,C'='       IGNORE IF GROUP SELECTED                     
         BE    XIT                                                              
         MVC   0(8,R3),=C'PRODUCT '                                             
         LR    R5,R3                                                            
         AH    R3,CODEDISP                                                      
         AH    R5,NAMEDISP                                                      
         MVC   0(3,R3),SPLPRO      USE CODE AND NAME FROM SCREEN                
         MVC   0(20,R5),SPLPRON                                                 
         B     XIT                                                              
         SPACE 1                                                                
GETPNAM  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   PKEYAM(3),NBACTAM                                                
         MVC   PKEYCLT,NBACTCLI    THEN USE CURRENT CLIENT                      
         MVC   PKEYPRD,SPRD                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   SAVEPRDN,PNAME                                                   
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES - ESTIMATE                                     
         SPACE 3                                                                
HOOKEST  NTR1                                                                   
         L     R3,ATHISH                                                        
         CLI   SPLESTH+5,0                                                      
         BE    XIT                                                              
         CLC   SPLEST(3),=C'NO,'                                                
         BE    HOOKEST1                                                         
         CLC   SPLEST(2),=C'NO'                                                 
         BE    XIT                                                              
         SPACE 1                                                                
HOOKEST1 MVC   0(8,R3),=C'ESTIMATE'                                             
         LR    R5,R3                                                            
         AH    R3,CODEDISP                                                      
         AH    R5,NAMEDISP                                                      
         MVC   0(3,R3),SPLEST      USE CODE AND NAME FROM SCREEN                
         MVC   0(24,R5),SPLESTN                                                 
         CLC   SPLEST(3),=C'NO,'   ESTIMATE FILTERS                             
         BNE   HOOKEST2                                                         
         MVC   0(5,R3),SPLEST+3                                                 
         MVC   0(24,R5),=CL24'ALL FILTERED'                                     
         B     HOOKALLX                                                         
         SPACE 1                                                                
HOOKEST2 CLI   NBSELESE,0                                                       
         BE    HOOKALLX                                                         
         MVC   WORK(29),=C'ESTIMATES 101 TO 102 COMBINED'                       
         EDIT  (1,NBSELEST),(3,WORK+10),WRK=DMCB                                
         EDIT  (1,NBSELESE),(3,WORK+17),WRK=DMCB                                
         GOTO1 SQUASHER,DMCB,WORK,29                                            
         L     R3,ATHISH                                                        
         MVC   0(40,R3),SPACES                                                  
         MVC   0(29,R3),WORK                                                    
         B     HOOKALLX                                                         
         EJECT                                                                  
*              HOOK - NETWORK DAYPART AND PACKAGE                               
         SPACE 3                                                                
HOOKNET  NTR1                                                                   
         MVC   H5+98(13),=C'NETWORK - ALL'                                      
         CLI   SPLNETH+5,0                                                      
         BE    XIT                                                              
         MVC   H5+108(8),SPLNET                                                 
         B     XIT                                                              
         SPACE 1                                                                
HOOKDPT  NTR1                                                                   
         MVC   H6+98(13),=C'DAYPART - ALL'                                      
         CLI   SPLDPTH+5,0                                                      
         BE    XIT                                                              
         MVC   H6+108(8),SPLDPTN                                                
         B     XIT                                                              
         SPACE 1                                                                
HOOKPACK NTR1                                                                   
         CLI   SPLDPTH+5,0                                                      
         BE    XIT                                                              
         MVC   H7+98(34),SPLPAKN                                                
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - CONTROL OF HEADINGS AND BOXES                             
         SPACE 3                                                                
HOOKBOX  NTR1                                                                   
         L     R5,ABOX                                                          
         USING BOXD,R5                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'B'                                                  
         BAS   RE,DETTITL                                                       
         MVC   BOXCOLS(132),DETBOX                                              
         MVC   H9(132),DETTIT1                                                  
         MVC   H10(132),DETTIT2                                                 
         DROP  R5                                                               
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
         MVI   0(R4),C'L'                                                       
         MVI   21(R4),C'C'                                                      
         MVC   1(12,R2),=C'PRODUCT NAME'                                        
         LA    R2,22(R2)                                                        
         MVC   0(5,R2),=C'MONTH'                                                
         LA    R2,5(R2)                                                         
         LA    R3,27(R3)                                                        
         LA    R4,27(R4)                                                        
         LA    R0,3                                                             
         SPACE 1                                                                
DETTITL2 BAS   RE,DPTIT            HEADINGS FOR EACH DAYPART                    
         AH    R2,BWIDTH                                                        
         AH    R3,BWIDTH                                                        
         AH    R4,BWIDTH                                                        
         BCT   R0,DETTITL2                                                      
         MVI   0(R4),C'R'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DPTIT    NTR1                                                                   
         MVI   0(R4),C'C'                                                       
         LA    R4,LENS                                                          
         ZIC   R5,NLENS                                                         
         SPACE 1                                                                
DPTIT2   MVI   3(R2),C'#'          NUMBER SIGN HERE                             
         EDIT  (1,0(R4)),(3,1(R3))                                              
         MVI   4(R3),C'S'                                                       
         LA    R2,5(R2)                                                         
         LA    R3,5(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,DPTIT2                                                        
         SPACE 1                                                                
         NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(7,WORK)                       
         MVC   1(7,R2),WORK        DEMO NAME TO FIRST LINE                      
         MVC   2(5,R3),=C'(GRP)'                                                
         MVC   14(4,R2),=C'COST'                                                
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R2),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS AND SPECS                                              
         SPACE 3                                                                
RELOC    DC    A(*)                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=36'                                    
         SPACE 1                                                                
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**SPECS*'                                                      
PBSPECS  SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,47,C'PRE-BUY EVALUATION REPORT'                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,54,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,124,PAGE                                                      
         DC    X'00'                                                            
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
         DC    C'*DPBUFF*'                                                      
DPBUFF   DC    260X'00'            ALLOW FOR 12 MONTHS + TOTAL                  
*                                  5 ACCUMS PER LINE = 20 BYTES                 
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*PLBUFF*'                                                      
PLBUFF   DC    22CL132' '          ALLOW UP TO 22 PRINT LINES                   
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*GRPLST*'                                                      
GROUPLST DC    880X'00'            PRODUCT GROUP LIST                           
         EJECT                                                                  
*              DSECT FOR ACCUMULATORS                                           
         SPACE 3                                                                
ACCUMD   DSECT                                                                  
ACENTRY  DS    0CL20               DSECT FOR ACCUMULATOR ENTRY                  
ACUN1    DS    F                   UNITS FOR SL1                                
ACUN2    DS    F                   UNITS FOR SL2                                
ACUN3    DS    F                   UNITS FOR SL3                                
ACGRP    DS    F                   POINTS (1DEC PLACE)                          
ACDOL    DS    F                   DOLLARS                                      
         EJECT                                                                  
*              DSECTS FOR PRE-BUY REPORT                                        
         SPACE 3                                                                
PRED     DSECT                     COMMON WITH EDIT                             
*              NETDEMOT AND                                                     
*              DEDBLOCK HERE                                                    
         PRINT OFF                                                              
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         SPACE 1                                                                
         PRINT ON                                                               
*                                  FIELDS SHARED WITH EDIT                      
SPACOPT  DS    CL1                                                              
BOXOPT   DS    CL1                                                              
LEFTOPT  DS    CL1                                                              
PFBOPT   DS    CL1                                                              
SCHOPT   DS    CL1                                                              
LENS     DS    XL3                                                              
NLENS    DS    XL1                                                              
         SPACE 1                                                                
*                                  LOCAL WORKING STORAGE                        
         SPACE 1                                                                
ADPBUFF  DS    A                   A(DAYPART BUFFER)                            
APLBUFF  DS    A                   A(PRINT LINE BUFFER)                         
RELO     DS    A                                                                
PERTYPE  DS    CL3                                                              
MLIST    DS    XL52                12 4-BYTE MONTH S/E + SPARE                  
NMONTHS  DS    XL1                 NUMBER OF MONTHS                             
ANYSORT  DS    CL1                                                              
DETDISP  DS    F                   DISPLACEMENT INTO DETAIL LINE                
BREAK    DS    CL12                                                             
BREAK2   DS    CL12                                                             
BRLEN    DS    XL1                                                              
BRLEN2   DS    XL1                                                              
CODEDISP DS    H                                                                
NAMEDISP DS    H                                                                
ATHISH   DS    A                                                                
DPCOUNT  DS    XL1                 NUMBER OF DAYPART BLOCKS (1-3)               
BWIDTH   DS    H                   WIDTH OF DAYPART BLOCK                       
SAVEGRP  DS    CL3                                                              
SAVEGRPN DS    CL24                                                             
SAVEG2N  DS    CL24                                                             
SAVEPRDN DS    CL20                                                             
         DS    0D                                                               
         SPACE 1                                                                
LASTREC  DS    CL36                                                             
SORTAREA DS    0CL36               SORT RECORD AREA                             
SGRP     DS    XL3                 PRODUCT GROUP                                
SPRD     DS    CL3                 PRODUCT CODE                                 
SDP      DS    CL8                 DAYPART                                      
SMONTH   DS    XL1                                                              
         DS    XL1                                                              
         SPACE 1                                                                
SACCUMS  DS    XL20                ACCUMULATORS                                 
         SPACE 2                                                                
DETTIT1  DS    CL132               TITLE LINE 1 FOR DETAIL                      
DETTIT2  DS    CL132               TITLE LINE 2 FOR DETAIL                      
DETBOX   DS    CL132               BOX COLUMNS FOR DETAILS                      
         DS    0F                                                               
*              SPGENCLT                                                         
*              SPGENPRD                                                         
*              SPGENPRG                                                         
*              NETINCLS                                                         
*              DDBIGBOX                                                         
*              NEWRIFFD                                                         
*              NEWRIFBD                                                         
         PRINT OFF                                                              
DUMMYD   DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIFBD                                                       
         SPACE 1                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI2BT  05/01/02'                                      
         END                                                                    

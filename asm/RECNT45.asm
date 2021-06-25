*          DATA SET RECNT45    AT LEVEL 016 AS OF 05/01/02                      
*PHASE T80245A,+0                                                               
*INCLUDE RECONDAT                                                               
*INCLUDE REVALDAT                                                               
         TITLE 'T80245 - REP SAR DISPLAY/EDIT'                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT45 (T80245) --- SAR DISPLAY/EDIT                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 23JUL97 SKU 4K CONTRACT SUPPORT                                 *             
* 22FEB96 IF PROPOSAL EXPANSION USED, DON'T ALLOW EDIT            *             
* 09OCT95 SKU 2K CONTRACT SUPPORT                                 *             
* 08JAN92 SKU STORED COMMENTS SUPPORT                             *             
* 14DEC90 EFJ LIMIT TO 6 DEMOS                                    *             
* 02AUG90 EFJ COMBINE WITH CNT46                                  *             
* 05APR90 EFJ USE GETEL MACRO                                     *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80245   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80245,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         ST    R5,TEMP                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,R7                                                       
         SPACE 1                                                                
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)                                                   
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)                                                   
         BE    EDIT                                                             
         DC    H'0'                                                             
DISP     GOTO1 VLOAD,DMCB,(X'80',0)                                             
         GOTO1 VFOUTBLK,DMCB,SARWKSH,SARLAST,0                                  
*                                                                               
         MVC   WORK(6),RCONDATE    CHECK FOR REVISED HEADER DATE                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP05                                                           
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    DISP05                                                           
         MVC   WORK(6),RCONRFLT                                                 
         DROP  R6                                                               
*                                                                               
DISP05   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXXMOD                                                           
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         LA    R2,SARWKSH                                                       
         L     R8,AFACILS                                                       
         GOTO1 =V(RECONDAT),DMCB,(R8),WORK,RSARBWKS,(R2),RR=TEMP                
         SPACE 1                                                                
         LA    R2,WORK3            PUT OUT LENGTHS AND CLASS                    
         MVC   WORK3,MYSPACES                                                   
         LA    R5,RSARRFRM                                                      
         LA    R3,6                                                             
         SPACE 1                                                                
DISP10   OC    0(2,R5),0(R5)                                                    
         BZ    DISP20              NO LENGTH IN THIS FIELD                      
         CH    R3,=H'6'                                                         
         BE    *+12                FIRST TIME                                   
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         ZIC   RE,1(R5)            LENGTH                                       
         MH    RE,=H'10'            LEFT ONE POSITION                           
         ZIC   RF,0(R5)            CLASS                                        
         AR    RE,RF                                                            
         EDIT  (RE),(5,0(R2)),1,ALIGN=LEFT                                      
         SH    R0,=H'2'                                                         
         AR    R2,R0               LENGTH OF OUTPUT                             
         CLC   0(2,R2),=C'.0'                                                   
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),MYSPACES                                                 
         SPACE 1                                                                
DISP20   LA    R5,2(R5)                                                         
         BCT   R3,DISP10                                                        
         MVC   SARLEN,WORK3                                                     
         EJECT                                                                  
         LA    R2,SARBKSH          BOOKS                                        
         CLC   RSARBKS(2),=C'DR'                                                
         BNE   DISP30                                                           
         MVC   SARBKS,MYSPACES                                                  
         MVC   SARBKS(2),=C'DR'                                                 
         MVC   SARDEM,MYSPACES                                                  
         B     DISP50                                                           
         SPACE 1                                                                
DISP30   DS    0H                                                               
         XC    DMCB+8(8),DMCB+8                                                 
         DROP  R6                                                               
         LR    R4,R6                                                            
         USING RSAREL,R4                                                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         ST    R6,DMCB+8                                                        
         MVI   DMCB+8,C'L'         YES-SET LABEL OPTION                         
         GOTO1 UNBOOK,DMCB,(6,RSARBKS),(R2),,0                                  
         SPACE 1                                                                
         LA    R2,SARDEMH          DEMOS                                        
         LA    R6,RBUYREC                                                       
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         SPACE 1                                                                
         LA    R5,WORK3                                                         
         XC    WORK3(30),WORK3                                                  
         MVC   0(L'RSARDEM,R5),RSARDEM        DEMOS + ENDING ZERO               
         LA    R3,6                                                             
DISP40   CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,DISP40                                                        
         SPACE 1                                                                
         LA    R5,WORK3                                                         
         SPACE 1                                                                
***********NOTE- DEMCON IS REALLY DEMOCON - SEE T80280***********               
         SPACE 1                                                                
         GOTO1 DEMCON,DMCB,(6,(R5)),(9,8(R2)),(0,DBLOCKD)                       
         DROP  R4,R6                                                            
         EJECT                                                                  
DISP50   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSAREL,R6           R6 STILL AT RSARELEM IF HERE                 
         LA    R2,WORK3            DAYPARTS                                     
         XC    WORK3(80),WORK3                                                  
         XC    DPTFLAG,DPTFLAG     0=1 CHAR DPT CODE                            
         LA    R5,RSARDPT                                                       
         SR    RE,RE               COUNTER -# DAYPARTS INPUTTED                 
         LA    R3,6                                                             
         SPACE 1                                                                
DISP60   LA    R8,DPTABLE                                                       
         OC    0(1,R5),0(R5)                                                    
         BZ    DISP90              NO DPT                                       
*                                                                               
         CLI   1(R5),X'C1'                                                      
         BL    DISP70              NO CHAR IN CPP PART OF FIELD                 
         CLI   1(R5),X'E9'                                                      
         BH    DISP70              NO CHAR IN CPP PART OF FIELD                 
*                                  3 CHAR DPT CODE                              
         MVI   DPTFLAG,1                                                        
         B     DISP80                                                           
*                                                                               
DISP70   CLC   3(1,R8),0(R5)                                                    
         BE    DISP80              ONE CHAR CODE IN RSARDPT                     
         CLI   0(R8),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R8,L'DPTABLE(R8)                                                 
         B     DISP70                                                           
*                                                                               
DISP80   CH    R3,=H'6'                                                         
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         CLI   DPTFLAG,0                                                        
         BNE   *+14                DPT IS THREE CHAR CODE                       
         MVC   0(3,R2),0(R8)       DPT 1 CHAR MOVE FROM TABLE                   
         B     *+10                                                             
         MVC   0(3,R2),0(R5)       MOVE DPT 3 CHAR FROM RSARDPT                 
         A     RE,=F'1'            INCREMENT COUNTER                            
         LA    R2,3(R2)                                                         
         SPACE 1                                                                
DISP90   LA    R5,3(R5)            NEXT DPT FIELD                               
         BCT   R3,DISP60                                                        
         ST    RE,DPTCNT           SAVE COUNTER                                 
         MVC   SARDPT,WORK3                                                     
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
         LA    R2,WORK3            GRPS                                         
         XC    WORK3(80),WORK3                                                  
         SR    RE,RE                                                            
         SR    RF,RF               ZERO COUNT- FOR # OF GRPS                    
         LA    R5,RSARBUD                                                       
         LA    R3,6                                                             
         SPACE 1                                                                
DISP100  C     RE,DPTCNT           COMPARE TO DAYPART COUNTER                   
         BE    DISP120                                                          
         CH    R3,=H'6'                                                         
         BNE   DISP110             NOT FIRST TIME                               
         SPACE 1                                                                
         MVI   0(R2),C'T'                                                       
         TM    0(R5),X'40'                                                      
         BZ    *+8                                                              
         MVI   0(R2),C'W'                                                       
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
DISP110  MVI   0(R2),C','          NOT FIRST                                    
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         CLC   1(2,R5),=X'0000'    IS IT ZERO?                                  
         BNE   *+8                                                              
         A     RF,=F'1'            INCREMENT ZERO COUNT                         
         EDIT  (B2,1(R5)),(5,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R2,R0                                                            
         A     RE,=F'1'                                                         
         SPACE 1                                                                
         LA    R5,3(R5)                                                         
         BCT   R3,DISP100                                                       
DISP120  CR    RF,RE               IS ZERO COUNT=DPTCOUNT                       
         BE    *+10                ALL ZEROS NO GRPS                            
         MVC   SARGRP,WORK3                                                     
         SPACE                                                                  
*                 DISPLAY GRP TOTAL-IF ANY                                      
         LA    R2,SARGTOT                                                       
         LA    R5,RSARGTOT                                                      
         OC    0(3,R5),0(R5)                                                    
         BZ    DISP130             NO TOTAL TO DISPLAY                          
*                                                                               
         MVI   0(R2),C'T'                                                       
         LA    R5,RSARGIND                                                      
         TM    0(R5),X'40'        WEEKLY                                        
         BZ    *+8                                                              
         MVI   0(R2),C'W'                                                       
         LA    R5,RSARGTOT                                                      
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         EDIT  (B3,0(R5)),(5,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         EJECT                                                                  
DISP130  DS    0H                  CPP                                          
         LA    R2,WORK3                                                         
         XC    WORK3(80),WORK3                                                  
         SR    RE,RE               CPP COUNT                                    
         SR    RF,RF               CPP ZERO COUNT                               
         LA    R5,RSARDPT                                                       
         LA    R3,6                                                             
*                                                                               
DISP140  C     RE,DPTCNT           COMPARE TO DAYPART COUNTER                   
         BE    DISP170                                                          
         CLC   1(2,R5),=X'0000'    ANY CPP?                                     
         BNE   *+12                                                             
         A     RF,=F'1'            INCREMENT ZERO COUNTER                       
         B     DISP150                                                          
         CLI   1(R5),X'C1'                                                      
         BL    DISP150             HAVE CPP                                     
         CLI   1(R5),X'E9'                                                      
         BH    DISP150             HAVE CPP                                     
         B     DISP160             NO CPP - 3 CHAR DPT CODE                     
*                                                                               
DISP150  CH    R3,=H'6'                                                         
         BE    *+12                1ST TIME-SKIP ','                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         EDIT  (B2,1(R5)),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         AR    R2,R0               R0 SIGN DGTS FROM EDIT                       
         A     RE,=F'1'                                                         
*                                                                               
DISP160  LA    R5,3(R5)            NEXT CPP FIELD                               
         BCT   R3,DISP140                                                       
DISP170  CR    RF,RE               WAS THERE ANY NONZERO INPUT                  
         BE    *+10                                                             
         MVC   SARCPP,WORK3                                                     
         SPACE                                                                  
         LA    R2,SARCTOT                                                       
         LA    R5,RSARCTOT                                                      
         OC    0(3,R5),0(R5)                                                    
         BZ    DISP180             NO TOTAL TO DISPLAY                          
         EDIT  (B3,0(R5)),(5,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         EJECT                                                                  
DISP180  DS    0H                  BUDGET                                       
         LA    R2,SARBGT                                                        
         LA    R5,RSARBGT          BUDGET FIELD- ONLY FOR NBC                   
         OC    0(4,R5),0(R5)       ANY BUDGET                                   
         BZ    DISP190                                                          
         EDIT  (B4,0(R5)),(8,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                     
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
DISP190  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP210                                                          
         LA    R2,SARCMTH                                                       
         SPACE 1                                                                
DISP200  MVC   WORK3(80),MYSPACES                                               
         ZIC   R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         BNP   DISP210                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
*                                                                               
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(1,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+10                                                             
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BAS   RE,NEXTEL                                                        
         BE    DISP200                                                          
         SPACE 1                                                                
DISP210  GOTO1 VFOUTBLK,DMCB,SARWKSH,SARLAST,1                                  
         B     EXXMOD                                                           
         EJECT                                                                  
EDIT     DS    0H                                                               
*          DATA SET RECNT46    AT LEVEL 082 AS OF 07/23/90                      
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
         CLC   CONACT,=C'ADDS'                                                  
         BE    EDIT10                                                           
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT05                                                           
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   FOR EXPANDED SAR ELEMENT ONLY                
         BL    EDIT05                                                           
         TM    RSARXFLG,X'04'      PROPOSAL EXPANSION USED                      
         BZ    EDIT05                                                           
         LA    R2,CONBACTH                                                      
         LA    R3,574                                                           
         B     ERROR                                                            
         DROP  R6                                                               
*                                                                               
EDIT05   DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'0B',RCONREC)                                    
*                                                                               
         CLC   SARWKS(6),=C'DELETE'                                             
         BE    SARDEL                                                           
         EJECT                                                                  
         USING RSAREL,R4                                                        
EDIT10   DS    0H                                                               
         CLC   =C'ACC-',CONBUY                                                  
         BE    EDIT13                                                           
         CLC   =C'GEN',CONADV                                                   
         BNE   EDIT15                                                           
         CLC   =C'ZZ',CONCAT                                                    
         BNE   EDIT15                                                           
EDIT13   L     R0,ABUYFH                                                        
         AR    R0,RA                                                            
         C     R0,LASTFLD          ANY INPUT BELOW BUY FLD?                     
         BNL   EXXMOD                                                           
EDIT15   LA    R4,WORK2                                                         
         XC    RSAREL(200),RSAREL                                               
         MVI   RSARCO,X'12'                                                     
         MVI   RSARLEN,120                                                      
         SPACE 1                                                                
         ZIC   R5,RCONWKS          NUMBER OF WEEKS                              
         LA    R2,SARWKSH                                                       
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVI   5(R2),3             FORCE START TO END IF NO INPUT               
         MVC   8(3,R2),=C'S-E'                                                  
         SPACE 1                                                                
         L     R8,AFACILS                                                       
*                                                                               
         MVC   WORK(6),RCONDATE    CHECK FOR REVISED HEADER DATE                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT18                                                           
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    EDIT18                                                           
         MVC   WORK(6),RCONRFLT                                                 
         DROP  R6                                                               
EDIT18   GOTO1 =V(REVALDAT),DMCB,(R8),WORK,(R2),RSARBWKS,RR=TEMP                
*                                                                               
         CLI   DMCB,0              TEST FOR ERROR                               
         BE    EDIT20              NONE                                         
         ZIC   R3,DMCB                                                          
         B     ERROR                                                            
         SPACE                                                                  
EDIT20   MVC   RSARWKS,DMCB+4                                                   
         EJECT                                                                  
         LA    R2,SARLENH          EDIT LENGTHS                                 
         GOTO1 VANY                                                             
         SPACE 1                                                                
         GOTO1 SCANNER,DMCB,(R2),(7,IOAREA),C',=,.'                             
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR               INVALID INPUT                                
         CH    R5,=H'6'                                                         
         BH    ERROR                                                            
         SPACE 1                                                                
         LA    R6,IOAREA                                                        
         LA    R1,RSARRFRM                                                      
         SPACE 1                                                                
EDIT30   TM    2(R6),X'80'                                                      
         BNO   ERROR               LENGTH NOT NUMERIC                           
         MVC   FULL,4(R6)          LENGTH                                       
         L     RE,FULL                                                          
         LTR   RE,RE                                                            
         BZ    ERROR               ZERO IS INVALID                              
         CH    RE,=H'240'          THIS IS TOO HIGH                             
         BH    ERROR                                                            
         STC   RE,1(R1)            SAVE IT IN RSARRFRM                          
         SPACE 1                                                                
         CLI   1(R6),0                                                          
         BE    EDIT40              NO CLASS NUMBER                              
         TM    3(R6),X'80'                                                      
         BNO   ERROR               CLASS NOT NUMERIC                            
         MVC   FULL,8(R6)          CLASS CODE                                   
         L     RE,FULL                                                          
         LTR   RE,RE                                                            
         BZ    ERROR               ZERO IS INVALID                              
         CH    RE,=H'9'                                                         
         BH    ERROR                                                            
         STC   RE,0(R1)            SAVE CLASS IN RSARRFRM                       
         SPACE 1                                                                
EDIT40   LA    R1,2(R1)            NEXT RSARRFRM                                
         LA    R6,32(R6)           NEXT SCAN BLOCK                              
         BCT   R5,EDIT30                                                        
         EJECT                                                                  
         LA    R2,SARBKSH          EDIT BOOKS                                   
         GOTO1 VANY                                                             
         MVC   RSARSRC,CONRTG      DEFAULT IS RATING SERVICE                    
         CLI   5(R2),2                                                          
         BNE   EDIT50                                                           
         CLC   SARBKS(2),=C'DR'                                                 
         BNE   EDIT50                                                           
         MVC   RSARBKS(2),=C'DR'                                                
         B     EDIT60                                                           
         SPACE 1                                                                
EDIT50   DS    0H                                                               
         XC    WORK3(2),WORK3                                                   
         MVC   WORK3+2(30),MYSPACES PRE-CLEAR AREA FOR LABEL ELEMENT            
         GOTO1 BOOKVAL,DMCB,(RSARSRC,0(R2)),(6,RSARBKS),               X        
               (C'L',SCANNER),WORK3+2                                           
         LA    R3,INVINP                                                        
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         CLI   DMCB+4,6                                                         
         BH    ERROR                                                            
         SPACE 1                                                                
         CLC   WORK3+2(30),MYSPACES                                             
         BE    EDIT60              NO BOOK LABELS                               
         ZIC   R1,DMCB+4                                                        
         MH    R1,=H'5'            LEN OF BOOK LABEL                            
         LA    R1,2(R1)            ELEMENT LENGTH                               
         STC   R1,WORK3+1                                                       
         MVI   WORK3,X'0B'         ADD ELEMENT TO CONTRACT                      
         GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
         SPACE 1                                                                
EDIT60   LA    R2,SARDEMH          EDIT DEMOS                                   
         CLC   RSARBKS(2),=C'DR'                                                
         BNE   EDIT70                                                           
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         B     EDIT110                                                          
         SPACE 1                                                                
EDIT70   GOTO1 VANY                                                             
         XC    RSARDEM,RSARDEM                                                  
         XC    WORK3(30),WORK3                                                  
         DROP  R4                                                               
         LA    R5,RBUYREC          AREA FOR DBLOCK                              
         USING DBLOCKD,R5                                                       
         USING RSAREL,R4                                                        
         LA    R4,WORK2                                                         
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         GOTO1 VDEMOVAL,DMCB,(R2),(6,WORK3),(C'Y',(R5))                         
         DROP  R5                                                               
         LA    R3,160              TOO MANY DEMOS                               
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
*                      ELIMINATE EOL MARKER (X'FF')                             
         ZIC   R5,DMCB+4                                                        
         MH    R5,=H'3'                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RSARDEM(0),WORK3                                                 
*                                                                               
* TEST PROFILE SET TO REQUIRE PRIMARY DEMO                                      
         TM    PROFILES+CNTPDEMB,CNTPDEMA                                       
         BZ    EDIT110                                                          
         LA    R1,RSARDEM                                                       
         LA    R0,8                                                             
         SPACE 1                                                                
EDIT100  TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    EDIT110                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,EDIT100                                                       
         LA    R3,PDEMERR                                                       
         B     ERROR                                                            
         EJECT                                                                  
EDIT110  LA    R2,SARDPTH          EDIT DAYPARTS                                
         GOTO1 VANY                                                             
         GOTO1 SCANNER,DMCB,(R2),(7,IOAREA),0                                   
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR               INVALID INPUT                                
         ST    R5,DPTCNT           SAVE DPT COUNTER                             
         CH    R5,=H'6'                                                         
         BH    ERROR               NO MORE THAN SIX                             
         SPACE 1                                                                
         LA    R6,IOAREA                                                        
         LA    R1,RSARDPT                                                       
         SPACE 1                                                                
EDIT120  CLI   1(R6),0                                                          
         BNE   ERROR               DELIMITER NO COMMA                           
         CLI   0(R6),3                                                          
         BH    ERROR               MORE THAN 3 POSITIONS                        
*                                                                               
         LA    R8,DPTABLE                                                       
EDIT130  CLC   0(3,R8),12(R6)                                                   
         BE    EDIT140                                                          
*                                                                               
         CLI   0(R8),X'FF'                                                      
         BE    ERROR               INVALID CODE                                 
         LA    R8,L'DPTABLE(R8)    NEXT DPTABLE                                 
         B     EDIT130                                                          
*                                                                               
EDIT140  MVC   0(1,R1),3(R8)       CODE INTO DPT FIELD                          
*                                                                               
         LA    R1,3(R1)            NEXT RSARDPT FIELD                           
         LA    R6,32(R6)           NEXT BLOCK                                   
         BCT   R5,EDIT120                                                       
         EJECT                                                                  
         XC    GRPENTRY,GRPENTRY   FLAG FOR GRP INPUT                           
         LA    R2,SARGRPH          EDIT GRP'S                                   
         CLI   5(R2),0                                                          
         BE    EDIT170             NO GRP'S                                     
         GOTO1 SCANNER,DMCB,(R2),(8,IOAREA),0                                   
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
*                                                                               
         LA    R3,208             ERR MSG- NEED T OR W                          
         LA    R6,IOAREA                                                        
         CLI   12(R6),C'W'                                                      
         BNE   *+12                                                             
         OI    RSARBUD,X'40'       WEEKLY                                       
         B     EDIT150                                                          
         CLI   12(R6),C'T'                                                      
         BNE   ERROR                                                            
         SPACE 1                                                                
         LA    R3,INVINP                                                        
         CLI   1(R6),0                                                          
         BNE   ERROR               SHOULD NOT BE A SECOND HALF                  
         SPACE 1                                                                
EDIT150  DC    0H'0'                                                            
         BCTR  R5,0                                                             
         LA    R3,205              ERROR 1-1 CORRESPONDENCE W/DPT               
         C     R5,DPTCNT           1-1 CORRESPONDANCE?                          
         BNE   ERROR                                                            
         LA    R6,IOAREA+32        SECOND BLOCK FOR NUMBERS                     
         LA    R3,INVINP                                                        
         SPACE 1                                                                
         CH    R5,=H'6'                                                         
         BH    ERROR               NOT MORE THAN 6                              
         LA    R1,RSARBUD                                                       
         SPACE 1                                                                
EDIT160  CLI   1(R6),0                                                          
         BNE   ERROR               NO SECOND HALF                               
         TM    2(R6),X'80'                                                      
         BZ    ERROR               NOT NUMERIC                                  
         MVC   FULL,4(R6)          GRP'S                                        
         L     RE,FULL                                                          
         C     RE,=F'65535'        THAT'S ALL I CAN FIT INTO 2 BYTES            
         BH    ERROR                                                            
         MVI   GRPENTRY,1          GRP ENTERED -FLAG NO TOTAL ALLOWED           
         MVC   1(2,R1),FULL+2      GRP'S TO RSARBUB                             
         OC    0(1,R1),RSARBUD     TURN ON REP, WEEKLY BITS                     
         LA    R6,32(R6)           NEXT BLOCK                                   
         LA    R1,3(R1)            NEXT RSARBUD                                 
         BCT   R5,EDIT160                                                       
         SPACE                                                                  
EDIT170  LA    R2,SARGTOTH         GRP TOTAL FIELD (NBC ONLY)                   
         CLI   5(R2),0             INPUT NOT MANDATORY                          
         BE    EDIT200                                                          
         LA    R3,206              ERROR MESG FOR TOTAL NOT ALLOWED             
         CLI   GRPENTRY,1          WAS GRP BREAKDOWN ENTERED?                   
         BE    ERROR               ONLY GRPS OR GRP TOTAL ALLOWED               
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
*                                                                               
         CLI   12(R6),C'W'                                                      
         BNE   EDIT180                                                          
         OI    RSARGIND,X'40'     WEEKLY                                        
         B     EDIT190                                                          
*                                                                               
EDIT180  LA    R3,208             ERROR MSG - NEED T OR W                       
         CLI   12(R6),C'T'                                                      
         BNE   ERROR                                                            
*                                                                               
EDIT190  LA    R6,IOAREA+32                                                     
         LA    R3,INVINP                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               NOT NUMERIC                                  
         MVC   RSARGTOT,5(R6)      USE BINARY VALUE- DON'T NEED BYTE            
*                                  4 WON'T BE THAT LARGE                        
         EJECT                                                                  
EDIT200  DS    0H                  EDIT CPP                                     
         XC    CPPENTRY,CPPENTRY   FLAG FOR CPP BREAKDOWN                       
         TM    SARCPPH+1,X'20'     IS IT PROTECTED?                             
         BO    EDIT240             SKIP CPP AND BUDGET VALIDATIONS              
*                                                                               
         LA    R2,SARCPPH                                                       
         CLI   5(R2),0             ANY INPUT                                    
         BE    EDIT220             NO CPP                                       
         GOTO1 SCANNER,DMCB,(R2),(7,IOAREA),0                                   
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR               INVALID INPUT                                
         LA    R3,205              ERR - NEED 1-1 CORRES W/DPT                  
         C     R5,DPTCNT           1-1 CORRESPONDENCE?                          
         BNE   ERROR                                                            
         LA    R3,INVINP                                                        
*                                                                               
         CH    R5,=H'6'                                                         
         BH    ERROR               NO MORE THAN SIX                             
*                                                                               
         LA    R6,IOAREA                                                        
         LA    R1,RSARDPT                                                       
*                                                                               
EDIT210  CLI   1(R6),0             SECOND HALF?                                 
         BNE   ERROR                                                            
         CLI   0(R6),4             CPP - 4 LONG                                 
         BH    ERROR               MORE THAN 4                                  
*                                                                               
         TM    2(R6),X'80'                                                      
         BZ    ERROR               NOT NUMERIC                                  
*                                  CPP DATA TO RSARDPT(CPP PART)                
         MVC   1(2,R1),6(R6)       STORED IN HEX(2 BYTES)                       
         MVI   CPPENTRY,1          FLAG BREAKDOWN ENTERED                       
*                                                                               
         LA    R1,3(R1)            NEXT RSARCPP FIELD                           
         LA    R6,32(R6)           NEXT BLOCK                                   
         BCT   R5,EDIT210                                                       
         SPACE                                                                  
EDIT220  LA    R2,SARCTOTH         CPP TOTAL FIELD (NBC ONLY)                   
         CLI   5(R2),0             ANY TOTAL ENTERED?                           
         BE    EDIT230             NOT MANDATORY                                
         LA    R3,207              ERROR MSG FOR TOTAL NOT ALLOWED              
         CLI   CPPENTRY,1          WAS CPP BREAKDOWN ENTERED                    
         BE    ERROR               ONLY BREAKDOWN OR TOTAL ALLOWED              
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               NOT NUMERIC                                  
         MVC   RSARCTOT,5(R6)      USE BINARY VALUE                             
*                                  EDIT BUDGET (NBC ONLY)                       
EDIT230  LA    R2,SARBGTH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BE    EDIT240                                                          
         GOTO1 SCANNER,DMCB,(R2),IOAREA,0                                       
         LA    R3,INVINP                                                        
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR                                                            
         LA    R6,IOAREA                                                        
         TM    2(R6),X'80'                                                      
         BZ    ERROR               NOT NUMERIC                                  
         MVC   RSARBGT,4(R6)       USE BINARY VALUE                             
         EJECT                                                                  
EDIT240  MVC   RSAREDT,TODAY                                                    
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT250                                                          
         SPACE 1                                                                
         MVC   RSAREDT,RSAREDT-RSAREL(R6)    SAVE SOME OLD STUFF                
         MVC   RSARSHR,RSARSHR-RSAREL(R6)                                       
         MVC   RSARRDT,RSARRDT-RSAREL(R6)                                       
         SPACE 1                                                                
EDIT250  GOTO1 VDELELEM,DMCB,(X'12',RCONREC)                                    
         GOTO1 VADDELEM,DMCB,RCONREC,(R4)                                       
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT260                                                          
         OI    4(R6),X'08'         SAR UPDATED                                  
         SPACE 1                                                                
EDIT260  GOTO1 VDELELEM,DMCB,(X'11',RCONREC)                                    
         SPACE 1                                                                
         LA    R2,SARCMTH          ADD COMMENTS                                 
         LA    R3,4                CHECK 4 COMMENT LINES                        
         SPACE 1                                                                
EDIT270  CLI   5(R2),0                                                          
         BE    EDIT280                                                          
*                                                                               
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+2(0),8(R2)     DATA TO ELEMENT                              
         AH    R1,=H'3'            ELEMENT LENGTH                               
         STC   R1,WORK+1                                                        
         MVI   WORK,X'11'                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
EDIT280  ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT COMMENT LINE                            
         BCT   R3,EDIT270                                                       
         EJECT                                                                  
         CLC   CONACT,=C'ADDS'                                                  
         BE    EXXMOD              IF ADDS T80211 CREATES POINTER               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         SPACE 1                                                                
         USING RCONATYP,R5                                                      
         LA    R5,WORK                                                          
         XC    WORK(40),WORK       CREATE POINTER                               
         MVI   RCONATYP,X'EC'                                                   
         MVC   RCONACON,TWACNUM    CONTRACT NUMBER 9'S COMP.                    
         MVC   RCONAREP,REPALPHA                                                
         MVC   KEY,RCONATYP                                                     
         SPACE 1                                                                
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    EDIT290             ALREADY THERE                                
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
         B     EDIT300                                                          
         SPACE 1                                                                
EDIT290  CLI   KEY+27,0                                                         
         BE    EDIT300             NOT DELETED                                  
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         SPACE 1                                                                
EDIT300  DC    0H'0'                                                            
         NI    TWASTAT,X'FD'       TURN OFF 02 - DISPLAYED                      
         NI    DMINBTS,X'F7'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         B     EXXMOD                                                           
         SPACE 1                                                                
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
         DROP  R5                                                               
SARDEL   GOTO1 VDELELEM,DMCB,(X'12',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'11',RCONREC)                                    
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         SPACE 1                                                                
         USING RCONATYP,R5                                                      
         LA    R5,WORK                                                          
         XC    WORK(40),WORK                                                    
         MVI   RCONATYP,X'EC'                                                   
         MVC   RCONACON,TWACNUM    CONTRACT NUMBER 9'S COMP.                    
         MVC   RCONAREP,REPALPHA                                                
         MVC   KEY,RCONATYP                                                     
         SPACE 1                                                                
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EDIT300                                                          
         SPACE 1                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,58     SAR DATA DELETED                             
         LA    R2,CONBACTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*    VALID DAYPARTS                                                             
DPTABLE  DS    0CL4                                                             
         DC    CL4'MNGM'            MORNING                                     
         DC    CL4'DAYD'            DAYTIME                                     
         DC    CL4'ELYE'            EARLY FRINGE                                
         DC    CL4'ENWR'            EARLY NEWS                                  
         DC    CL4'ACCA'            PRIME ACCESS                                
         DC    CL4'LNWT'            LATE NEWS                                   
         DC    CL4'LTEL'            LATE FRINGE                                 
         DC    CL4'WKDW'            WEEKEND                                     
         DC    CL4'KIDK'            KIDS                                        
         DC    CL4'FRGF'            FRINGE                                      
         DC    CL4'NWSN'            NEWS                                        
         DC    CL4'PRIP'            PRIME                                       
         DC    CL4'MOVV'            MOVIES                                      
         DC    CL4'SPES'            SPECIALS                                    
         DC    CL4'SPOJ'            SPORTS                                      
         DC    CL4'SPSO'            SOAPS                                       
         DC    CL4'COMU'            COMPETITIVE                                 
         DC    CL4'LOCX'            LOCAL                                       
         DC    X'FF'                                                            
*                                                                               
DPTFLAG  DS    XL1                                                              
DPTCNT   DS    F                                                                
GRPENTRY DS    XL1                                                              
CPPENTRY DS    XL1                                                              
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTF5D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016RECNT45   05/01/02'                                      
         END                                                                    

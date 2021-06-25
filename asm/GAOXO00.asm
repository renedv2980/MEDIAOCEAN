*          DATA SET GAOXO00    AT LEVEL 004 AS OF 05/01/02                      
*PHASE TB0100A                                                                  
         TITLE 'NOUGHTS AND CROSSES GAME'                                       
         PRINT NOGEN                                                            
OXOCNTL  CSECT                                                                  
         NMOD1 WORKX-WORKD,GAOXO00,RR=R7                                        
         USING WORKD,RC                                                         
         ST    R1,SAVER1                                                        
         L     RA,4(R1)            RA=A(TWA)                                    
         USING GAOXOFFD,RA                                                      
         SPACE 1                                                                
         L     RE,16(R1)           SAVE AND EXTRACT LANGUAGE CODE               
         USING COMFACSD,RE                                                      
         MVC   AGETFACT,CGETFACT                                                
         GOTO1 AGETFACT,PLIST,0                                                 
         L     RE,0(R1)                                                         
         MVC   LANG,FALANG-FACTSD(RE)                                           
         SPACE 1                                                                
         L     RF,=V(OXOGAME)      RELOCATE OXO GAME MODULE                     
         AR    RF,R7                                                            
         ST    RF,AOXOGAME                                                      
         SPACE 1                                                                
         LA    R1,OXOSAVE          POINT TO SAVE STORAGE                        
         ST    R1,PARA                                                          
         LA    R1,PARA                                                          
         EJECT                                                                  
* CONTROL OF CALLS TO OXOGAME                                                   
*                                                                               
         CLI   16(RA),X'FF'        USE TWA+16 AS FTS                            
         BE    OX2                                                              
         MVI   16(RA),X'FF'                                                     
         ZAP   YSCORE,=P'0'        INITIALISE SCOREBOARD                        
         ZAP   MSCORE,=P'0'                                                     
         MVI   OLDLA,2             HIS START FIRST                              
         MVI   OLDLB,3                                                          
         B     OX4                                                              
         SPACE 1                                                                
OX2      CLI   OLDLA,2             HE IS STARTING GAME                          
         BNE   OX6                                                              
         SPACE 1                                                                
OX4      MVI   TURN,C'M'           MY TURN NEXT                                 
         MVI   LETTER,C'S'                                                      
         BASR  RE,RF                                                            
         B     OX8                                                              
         SPACE 1                                                                
OX6      CLI   OLDLA,3             I AM STARTING GAME                           
         BNE   OX8                                                              
         MVI   LETTER,C'S'                                                      
         BASR  RE,RF                                                            
         MVI   TURN,C'Y'                                                        
         MVI   OXOLET,C'Y'                                                      
         SPACE 1                                                                
OX8      MVC   LETTER(1),OXOLET                                                 
         BASR  RE,RF                                                            
         EJECT                                                                  
* TEST RETURNS FROM OXOGAME                                                     
*                                                                               
         CLI   LETTER,1            0/1 = GAME IN PROGRESS                       
         BH    OX10                                                             
         MVC   NEWLA(1),LETTER                                                  
         MVC   NEWLB(1),OLDLB                                                   
         B     FORMAT                                                           
         SPACE 1                                                                
OX10     MVI   NEWLA,2             END OF GAME - CHECK WHO STARTS NEXT          
         CLI   TURN,C'M'                                                        
         BNE   *+8                                                              
         MVI   NEWLA,3                                                          
         CLI   LETTER,2            I WON GAME                                   
         BNE   OX12                                                             
         AP    MSCORE,=P'1'                                                     
         MVC   OXOMSCR+10(4),=X'40202021'                                       
         ED    OXOMSCR+10(4),MSCORE+2                                           
         FOUT  OXOMSCRH                                                         
         MVI   NEWLB,0                                                          
         B     FORMAT                                                           
         SPACE 1                                                                
OX12     CLI   LETTER,3            HE WON GAME                                  
         BNE   OX14                                                             
         AP    YSCORE,=P'1'                                                     
         MVC   OXOYSCR+10(4),=X'40202021'                                       
         ED    OXOYSCR+10(4),YSCORE+2                                           
         FOUT  OXOYSCRH                                                         
         MVI   NEWLB,1                                                          
         B     FORMAT                                                           
         SPACE 1                                                                
OX14     MVI   NEWLB,2             ITS A DRAW                                   
         EJECT                                                                  
* OUTPUT HEADINGS WHERE NECESSARY                                               
*                                                                               
FORMAT   CLC   OLDLA,NEWLA         LINE 1                                       
         BE    OX16                                                             
         MVC   OLDLA,NEWLA                                                      
         SR    R2,R2                                                            
         IC    R2,NEWLA                                                         
         MH    R2,=H'60'                                                        
         LA    RE,LINE1                                                         
         CLI   LANG,3                                                           
         BNE   *+8                                                              
         LA    RE,LINE1G                                                        
         CLI   LANG,4                                                           
         BNE   *+8                                                              
         LA    RE,LINE1F                                                        
         AR    R2,RE                                                            
         FOUT  OXOHED1H,(R2),60                                                 
         SPACE 1                                                                
OX16     CLC   OLDLB,NEWLB         LINE 2                                       
         BE    OX18                                                             
         MVC   OLDLB,NEWLB                                                      
         SR    R2,R2                                                            
         IC    R2,NEWLB                                                         
         MH    R2,=H'60'                                                        
         LA    RE,LINE2                                                         
         CLI   LANG,3                                                           
         BNE   *+8                                                              
         LA    RE,LINE2G                                                        
         CLI   LANG,4                                                           
         BNE   *+8                                                              
         LA    RE,LINE2F                                                        
         AR    R2,RE                                                            
         FOUT  OXOHED2H,(R2),60                                                 
         SPACE 1                                                                
OX18     CLC   OLDBOARD(3),NEWBOARD                                             
         BE    OX20                                                             
         MVC   OLDBOARD(3),NEWBOARD                                             
         LA    R2,OLDBOARD                                                      
         LA    R3,OXOFRSTH                                                      
         BAS   RE,BIGOUT                                                        
         SPACE 1                                                                
OX20     CLC   OLDBOARD+3(3),NEWBOARD+3                                         
         BE    OX22                                                             
         MVC   OLDBOARD+3(3),NEWBOARD+3                                         
         LA    R2,OLDBOARD+3                                                    
         LA    R3,OXOSCNDH                                                      
         BAS   RE,BIGOUT                                                        
         SPACE 1                                                                
OX22     CLC   OLDBOARD+6(3),NEWBOARD+6                                         
         BE    OXEXT                                                            
         MVC   OLDBOARD+6(3),NEWBOARD+6                                         
         LA    R2,OLDBOARD+6                                                    
         LA    R3,OXOTHRDH                                                      
         BAS   RE,BIGOUT                                                        
         SPACE 1                                                                
OXEXT    EQU   *                                                                
         OI    OXOLETH+6,X'40'                                                  
         XMOD1 1                                                                
         EJECT                                                                  
* ROUTINE TO PUT BIGLETTERS ONTO SCREEN                                         
*                                                                               
BIGOUT   NTR                                                                    
         LR    R4,R3               FIRST FOUT THE LINES                         
         LA    R5,6                                                             
         SPACE 2                                                                
BIG2     FOUT  (R4)                                                             
         LA    R4,75(R4)                                                        
         BCT   R5,BIG2                                                          
         LA    R3,8(R3)            SET UP FOR THREE LETTERS                     
         LA    R4,3                                                             
         SPACE 1                                                                
BIG4     LR    R5,R3               SET UP FOR SIXLINES                          
         LA    R6,6                                                             
         LA    R7,SPACES                                                        
         CLI   0(R2),C' '          KEY TO A BIG LETTER                          
         BE    BIG6                                                             
         LA    R7,O                                                             
         CLI   0(R2),C'O'                                                       
         BE    BIG6                                                             
         LA    R7,X                                                             
         SPACE 1                                                                
BIG6     MVC   0(11,R5),0(R7)                                                   
         LA    R5,75(R5)                                                        
         LA    R7,11(R7)                                                        
         BCT   R6,BIG6                                                          
         LA    R2,1(R2)            BUMP TO NEXT LETTER                          
         LA    R3,14(R3)                                                        
         BCT   R4,BIG4                                                          
         XIT                                                                    
         EJECT                                                                  
* BIG O AND X FOR OUTPUT                                                        
*                                                                               
SPACES   DC    CL66' '                                                          
         SPACE 1                                                                
O        DC    CL11'   OOOOO   '                                                
         DC    CL11' OO     OO '                                                
         DC    CL11'OO       OO'                                                
         DC    CL11'OO       OO'                                                
         DC    CL11' OO     OO '                                                
         DC    CL11'   OOOOO   '                                                
         SPACE 1                                                                
X        DC    CL11'  XX   XX  '                                                
         DC    CL11'   XX XX   '                                                
         DC    CL11'    XXX    '                                                
         DC    CL11'    XXX    '                                                
         DC    CL11'   XX XX   '                                                
         DC    CL11'  XX   XX  '                                                
         EJECT                                                                  
* OUTPUT MESSAGES FOR OXO                                                       
*                                                                               
LINE1    DC    CL60'Game is in progress - Enter a letter from key'              
         DC    CL60'Invalid input letter - Please reinput'                      
         DC    CL60'Its your turn to start - Enter a letter from key'           
         DC    CL60'Its my turn to start - Please enter an ''S'''               
*                                                                               
LINE1G   DC    CL60'Das Spiel ist im Gange - Tragen Sie einen BuchstabeX        
               n ein'                                                           
         DC    CL60'Ungultiger Buchstabe - Bitte einen neuen eintragen'         
         DC    CL60'Sie durfen anfangen - Tragen Sie einen Buchstaben eX        
               in'                                                              
         DC    CL60'Ich darf anfangen - Bitte einen ''S'' eintragen'            
*                                                                               
LINE1F   DC    CL60'Le jou continue - Entre une lettre du clef'                 
         DC    CL60'La lettre est invalide - Entrez encore s''il vous pX        
               lait'                                                            
         DC    CL60'C''est toi a commencer - Entrez une lettre du clef'         
         DC    CL60'C''est moi a commencer - Entrez une ''S'''                  
         SPACE 2                                                                
LINE2    DC    CL60'I won the last game - Hard luck'                            
         DC    CL60'You won the last game - Well done'                          
         DC    CL60'The last game ended as a draw'                              
         DC    CL60'This is the first game'                                     
*                                                                               
LINE2G   DC    CL60'Ich habe gewonnen - Pech!'                                  
         DC    CL60'Sie haben gewonnen - Gut gespielt'                          
         DC    CL60'Das letzte Spiel war unentschieden'                         
         DC    CL60'Hier ist das erste Spiel'                                   
*                                                                               
LINE2F   DC    CL60'J''ai gagner la jou dernier'                                
         DC    CL60'Vous avez gagner la jou dernier - Tres bien'                
         DC    CL60'Rien personne avez gagner la jou dernier'                   
         DC    CL60'C''est la premier jou'                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* REQUIRES ONE PARAMETER WHICH IS THE ADDR OF A DATA AREA                       
* DEFINED BY THE DSECT OXODATA                                                  
* THE FIRST BYTE OF THIS AREA IS THE INPUT FROM THE PLAYER                      
* THE REMAINDER OF THE AREA IS USED TO STORE INFO ON THE                        
* GAME AS IT PROGRESSES                                                         
*                                                                               
OXOGAME  CSECT                                                                  
         NMOD1 0,OXOGAME                                                        
         L     RA,0(R1)                                                         
         USING OXODATA,RA                                                       
         SPACE 1                                                                
* VALIDATE INPUT CHR                                                            
*                                                                               
OXGNEXT  CLI   OXDINCHR,C'S'                                                    
         BE    OXGS                                                             
         CLI   OXDINCHR,C'Y'                                                    
         BE    OXGY                                                             
         CLI   OXDINCHR,C'A'                                                    
         BL    OXGINV                                                           
         CLI   OXDINCHR,C'J'                                                    
         BH    OXGINV                                                           
         B     OXGAJ                                                            
         SPACE 1                                                                
* INPUT IS S - SO INITIALISE BOARD                                              
*                                                                               
OXGS     MVI   OXDINCHR,C' '                                                    
         MVC   OXDINCHR+1(35),OXDINCHR                                          
         XC    OXDRCDV(36),OXDRCDV                                              
         B     OXGINIT                                                          
         SPACE 1                                                                
* INPUT IS Y - ONLY VALID AFTER AN S                                            
*                                                                               
OXGY     CLI   OXDWHOGO,C' '                                                    
         BNE   OXGINV                                                           
         CLI   OXDCNT+1,X'00'                                                   
         BNE   OXGINV                                                           
         B     OXGMYGO                                                          
         EJECT                                                                  
* INPUT IS A SQUARE LETTER - SO VALIDATE AND PUT IN BOARD                       
*                                                                               
OXGAJ    MVN   OXDSQR+1(1),OXDINCHR                                             
         NC    OXDSQR,=X'000F'                                                  
         LH    3,OXDSQR                                                         
         BCTR  3,0                                                              
         STH   3,OXDSQR                                                         
         LA    4,OXDBRD(3)                                                      
         CLI   0(4),C' '                                                        
         BNE   OXGINV                                                           
         MVI   OXDWHOGO,C'O'                                                    
         BAS   9,OXGINSRT                                                       
         BAS   9,OXGVALUS                                                       
         CLI   OXDCNT+1,X'02'                                                   
         BH    OXGHHW                                                           
         MVC   OXDHF,OXDSQR                                                     
         B     OXGMYGO                                                          
         SPACE 1                                                                
* HE HAS WON IF A VALUE 26 IS PRESENT IN OXDRCDV                                
*                                                                               
OXGHHW   LA    3,26                                                             
         LA    4,26                                                             
         LA    5,0                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BNM   OXGHEWON                                                         
         EJECT                                                                  
* ITS MY TURN TO GO                                                             
*                                                                               
OXGMYGO  MVI   OXDWHOGO,C'X'                                                    
         CLI   OXDCNT+1,X'09'                                                   
         BE    OXGDRAW                                                          
         CLI   OXDCNT+1,X'01'                                                   
         BNH   OXGMYFST                                                         
         CLI   OXDCNT+1,X'02'                                                   
         BE    OXGPLAN                                                          
         SPACE 1                                                                
* I CAN WIN IF A VALUE 7,8,9 IS PRESENT IN OXDRCDV                              
*                                                                               
OXGCIW   LA    3,7                                                              
         LA    4,9                                                              
         LA    5,1                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BNM   OXGIWON                                                          
         SPACE 1                                                                
* HE CAN WIN IF A VALUE 10,11,12 IS PRESENT IN OXDRCDV                          
*                                                                               
OXGCHW   LA    3,10                                                             
         LA    4,12                                                             
         LA    5,1                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BNM   OXGHISGO                                                         
         SPACE 1                                                                
* HE WILL WIN IF OXO IS IN A DIAGONAL AFTER 3 GOES                              
*                                                                               
OXGWHW   CLI   OXDCNT+1,X'03'                                                   
         BNE   OXGWIW                                                           
         LA    3,20                                                             
         LA    4,20                                                             
         LA    5,0                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BM    OXGWIW                                                           
         CLI   OXDSELRN,X'06'                                                   
         BL    OXGWIW                                                           
         BAS   9,OXGTIMEB                                                       
         SLL   1,29                                                             
         SRL   1,29                               R1=0 THRU 7                   
         O     1,=F'1'                            R1=1,3,5,OR 7                 
         STH   1,OXDSQR                                                         
         B     OXGHISGO                                                         
         SPACE 1                                                                
* I WILL WIN IF I AM OPERATING UNDER A PLAN                                     
*                                                                               
OXGWIW   CLI   OXDCNT+1,X'04'                                                   
         BNE   OXGXTOXX                                                         
         CLI   OXDPLAN+1,X'00'                                                  
         BE    OXGXTOXX                                                         
         MVC   OXDSQR,OXDPGO5                                                   
         B     OXGHISGO                                                         
         SPACE 1                                                                
* TRY TO FIND ROW WITH ONLY ONE X & PUT IN ANOTHER X                            
*                                                                               
OXGXTOXX LA    3,01                                                             
         LA    4,03                                                             
         LA    5,2                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BNM   OXGHISGO                                                         
         SPACE 1                                                                
* TRY TO FIND BLANK ROW & PUT IN AN X                                           
*                                                                               
OXGBTX   LA    3,00                                                             
         LA    4,00                                                             
         LA    5,3                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BNM   OXGHISGO                                                         
         SPACE 1                                                                
* TRY TO FIND ROW WITH ONLY ONE O & PUT IN AN X                                 
*                                                                               
OXGOTOX  LA    3,04                                                             
         LA    4,06                                                             
         LA    5,2                                                              
         BAS   9,OXGSELCT                                                       
         LTR   1,1                                                              
         BNM   OXGHISGO                                                         
         SPACE 1                                                                
         B     OXGDRAW                                                          
         EJECT                                                                  
* ITS MY FIRST X - USE TIME TO DETERMINE SQUARE CHOICE                          
*                                                                               
OXGMYFST BAS   9,OXGTIMEP                                                       
         SLL   1,24                                                             
         SRL   1,29                                                             
         SLL   1,1                                R1=0,2,4,6,OR 8               
         CLI   OXDCNT+1,X'00'                                                   
         BE    OXGMYF2                                                          
         CLI   OXDHF+1,X'04'                                                    
         BE    OXGMYF1                                                          
         LA    1,4                                HE DIDNT START AT MID         
         B     OXGMYF2                            SO I WILL                     
OXGMYF1  CH    1,=H'4'                            HE STARTED AT MID             
         BE    OXGMYFST                           SO I CANT                     
OXGMYF2  STH   1,OXDSQR                           I START AT A,C,E,G,I          
OXGMYF3  STH   1,OXDMF                                                          
         B     OXGHISGO                                                         
         EJECT                                                                  
* I STARTED FIRST & ITS MY SECOND GO - CAN I SET UP A PLAN                      
*                                                                               
OXGPLAN  CLI   OXDMF+1,X'04'                                                    
         BNE   OXGPL2                                                           
         TM    OXDHF+1,X'01'                                                    
         BC    8,OXGPLX                                                         
         SPACE 1                                                                
*  PLAN#1 I STARTED IN CENTRE & HE DIDNT REPLY IN CORNER                        
*                                                                               
OXGPL1   BAS   9,OXGTIMEB                                                       
         SLL   1,31                                                             
         SRL   1,31                               R1= 0 OR 1                    
         AH    1,OXDHF                            R1= INDEX TO PL1GOS35         
         IC    3,PL1GOS35(1)                                                    
         MVI   OXDPLAN+1,X'01'                                                  
OXGPL11  N     3,=F'255'                          R3=R5=GOS 3 & 5               
         LR    5,3                                                              
         SRL   3,4                                R3= GO 3                      
         SLL   5,28                                                             
         SRL   5,28                               R5= GO 5                      
         STH   3,OXDSQR                                                         
         STH   5,OXDPGO5                                                        
         CLI   OXDPGO5+1,X'0F'                                                  
         BNE   OXGHISGO                                                         
         MVI   OXDPLAN+1,X'00'                                                  
         B     OXGHISGO                                                         
OXGPL2   CLI   OXDHF+1,X'04'                                                    
         BE    OXGPLX                                                           
         SPACE 1                                                                
* PLAN#2 I STARTED IN CORNER & HE DIDNT REPLY IN CENTRE                         
*                                                                               
         LA    3,PL2BRDS                                                        
         LA    4,28                                                             
OXGPL21  CLC   0(9,3),OXDBRD                                                    
         BE    OXGPL22                                                          
         LA    3,9(3)                                                           
         BCT   4,OXGPL21                                                        
         B     OXGLOGIC                                                         
OXGPL22  SH    4,=H'28'                                                         
         LPR   4,4                                                              
         MH    4,=H'5'                            R4= INDEX TO PL2GOS35         
         BAS   9,OXGTIMEP                                                       
         SLL   1,24                                                             
         SRL   1,29                                                             
         LA    1,0(1,4)                                                         
         IC    3,PL2GOS35(1)                                                    
         MVI   OXDPLAN+1,X'02'                                                  
         B     OXGPL11                                                          
OXGPLX   B     OXGCIW                                                           
         EJECT                                                                  
OXGTIMEB EQU   *                                                                
         TBIN  SECS                                                             
         BR    9                                                                
         SPACE 1                                                                
OXGTIMEP EQU   *                                                                
         TBIN  SECS                                                             
         CVD   1,OXDTIM                                                         
         L     1,OXDTIM+4                                                       
         BR    9                                                                
         EJECT                                                                  
* INSERT CHR @ OXDWHOGO INTO SQ # OXDSQR & UP THE OXDCNT                        
*                                                                               
OXGINSRT EQU   *                                                                
         LH    3,OXDSQR                                                         
         LA    4,OXDBRD(3)                                                      
         MVC   0(1,4),OXDWHOGO                                                  
         SLA   3,2                                                              
         LA    4,SQMAP(3)                                                       
         LA    5,OXDRCD                                                         
         LA    7,4                                                              
OXGIN1   CLI   0(4),X'FF'                                                       
         BE    OXGIN2                                                           
         SR    6,6                                                              
         IC    6,0(4)                                                           
         LA    6,0(6,5)                                                         
         MVC   0(1,6),OXDWHOGO                                                  
OXGIN2   LA    4,1(4)                                                           
         BCT   7,OXGIN1                                                         
         LH    3,OXDCNT                                                         
         LA    3,1(3)                                                           
         STH   3,OXDCNT                                                         
         BR    9                                                                
         EJECT                                                                  
* FROM DATA @ OXDRCD SET UP OXDRCDV                                             
*                                                                               
OXGVALUS EQU   *                                                                
         LA    3,SQCOMBS                                                        
         LA    4,OXDRCD                                                         
         LA    5,OXDRCDV                                                        
         LA    6,8                                                              
OXGVAL1  LR    15,3                                                             
         LA    14,27                                                            
         LR    7,14                                                             
OXGVAL2  CLC   0(3,15),0(4)                                                     
         BE    OXGVAL3                                                          
         LA    15,3(15)                                                         
         BCT   14,OXGVAL2                                                       
         B     OXGLOGIC                                                         
OXGVAL3  SR    14,7                                                             
         LPR   14,14                                                            
         STC   14,0(5)                                                          
         LA    4,3(4)                                                           
         LA    5,1(5)                                                           
         BCT   6,OXGVAL1                                                        
         BR    9                                                                
         EJECT                                                                  
* SELECT ROWS WITH VALUES GE R#3 & LE R#4. OPTION IN R#5                        
*                                                                               
         CNOP  0,4                                                              
         DS    F                                                                
OXGSELCT ST    9,OXGSELCT-4                                                     
         SR    14,14                                                            
         SR    15,15                                                            
         SR    7,7                                                              
         SPACE 1                                                                
OXGSEL1  IC    7,OXDRCDV(14)                                                    
         CR    7,3                                                              
         BL    OXGSEL2                                                          
         CR    7,4                                                              
         BH    OXGSEL2                                                          
         STC   07,OXDSELRV(15)                                                  
         STC   14,OXDSELRN(15)                                                  
         LA    15,1(15)                                                         
OXGSEL2  LA    14,1(14)                                                         
         CH    14,=H'7'                                                         
         BNH   OXGSEL1                                                          
         SPACE 1                                                                
         LTR   15,15                                                            
         BNZ   OXGSEL3                                                          
         LH    1,=H'-1'                                                         
         B     OXGSELX                                                          
         SPACE 1                                                                
OXGSEL3  LTR   5,5                                                              
         BNZ   OXGSEL4                                                          
         SR    1,1                                                              
         B     OXGSEL9                                                          
         SPACE 1                                                                
OXGSEL4  LA    1,OXDSELRN                         REPLICATE ROW NOS             
         LA    6,OXDSELRN(15)                                                   
         LA    7,OXDSELRN+7                                                     
OXGSEL5  CR    6,7                                                              
         BH    OXGSEL6                                                          
         MVC   0(1,6),0(1)                                                      
         LA    1,1(1)                                                           
         LA    6,1(6)                                                           
         B     OXGSEL5                                                          
         SPACE 1                                                                
OXGSEL6  LA    1,OXDSELRV                         REPLICATE ROW VALUES          
         LA    6,OXDSELRV(15)                                                   
         LA    7,OXDSELRV+7                                                     
OXGSEL7  CR    6,7                                                              
         BH    OXGSEL8                                                          
         MVC   0(1,6),0(1)                                                      
         LA    1,1(1)                                                           
         LA    6,1(6)                                                           
         B     OXGSEL7                                                          
         SPACE 1                                                                
OXGSEL8  BAS   9,OXGTIMEB                                                       
         SLL   1,29                                                             
         SRL   1,29                               R1 = 0 THRU 7                 
         SPACE 1                                                                
OXGSEL9  SR    6,6                                                              
         SR    7,7                                                              
         IC    6,OXDSELRV(1)                                                    
         IC    7,OXDSELRN(1)                      R7=A ROW # FOUND              
         SR    6,3                                R6=REL POS IN ROW             
         MH    7,=H'3'                                                          
         SPACE 1                                                                
         CH    5,=H'1'                                                          
         BNH   OXGSEL11                                                         
         SPACE 1                                                                
         CH    5,=H'2'                                                          
         BNE   OXGSEL10                                                         
         SPACE 1                                                                
OXGSEL91 SLL   6,1                                SELECT 1 FROM 2 OPTN          
         BAS   9,OXGTIMEB                                                       
         SLL   1,31                                                             
         SRL   1,31                               R1 = 0 OR 1                   
         AR    6,1                                                              
         IC    6,OXDP1F2(6)                                                     
         B     OXGSEL11                                                         
OXGSEL10 LA    6,1                                SELECT 1 FROM 3 OPTN          
         B     OXGSEL91                                                         
         SPACE 1                                                                
OXGSEL11 AR    7,6                                                              
         SR    1,1                                                              
         IC    1,SQMAPINV(7)                                                    
         STH   1,OXDSQR                                                         
         SPACE 1                                                                
OXGSELX  L     9,OXGSELCT-4                                                     
         BR    9                                                                
         EJECT                                                                  
* GAMES NOT OVER SO OUTPUT BOARD                                                
*                                                                               
OXGHISGO BAS   9,OXGINSRT                                                       
         CLI   OXDCNT+1,X'09'                                                   
         BE    OXGDRAW                                                          
         MVI   OXDINCHR,0                                                       
         B     OXGOPUT                                                          
         SPACE 1                                                                
* INPUT IS INVALID - SO SET UP ERROR MSG                                        
*                                                                               
OXGINV   MVI   OXDINCHR,1                                                       
         B     OXGOPUT                                                          
         SPACE 1                                                                
* I WON - SO BREAK THE NEWS                                                     
*                                                                               
OXGIWON  BAS   9,OXGINSRT                                                       
         MVI   OXDINCHR,2                                                       
         B     OXGOPUT                                                          
         SPACE 1                                                                
* HE WON - SO LOOK FOR A LOGIC ERROR !                                          
*                                                                               
OXGHEWON MVI   OXDINCHR,3                                                       
         B     OXGOPUT                                                          
         SPACE 1                                                                
* ITS A DRAW - SO TELL HIM                                                      
*                                                                               
OXGDRAW  MVI   OXDINCHR,4                                                       
         B     OXGOPUT                                                          
         SPACE 1                                                                
* INPUT IS S - SO SET UP INSTRUCTIONS                                           
*                                                                               
OXGINIT  MVI   OXDINCHR,5                                                       
         B     OXGOPUT                                                          
         SPACE 1                                                                
* SOMETHING WENT WRONG SOMEWHERE!                                               
*                                                                               
OXGLOGIC DC    H'0'                                                             
         SPACE 1                                                                
OXGOPUT  XMOD1 1                                                                
         EJECT                                                                  
SQMAP    DS    0CL36                                                            
SQA      DC    XL4'000912FF'                      0,9,18                        
SQB      DC    XL4'010CFFFF'                      1,12                          
SQC      DC    XL4'020F15FF'                      2,15,21                       
SQD      DC    XL4'030AFFFF'                      3,10                          
SQE      DC    XL4'040D1316'                      4,13,19,22                    
SQF      DC    XL4'0510FFFF'                      5,16                          
SQG      DC    XL4'060B17FF'                      6,11,23                       
SQH      DC    XL4'070EFFFF'                      7,14                          
SQI      DC    XL4'081114FF'                      8,17,20                       
         SPACE 1                                                                
SQMAPINV DC    XL24'000102030405060708000306010407020508000408020406'           
         SPACE 1                                                                
SQCOMBS  DS    0CL81                              POSSIBLE 3 SQR COMBS          
         DC    CL03'   '                          00-00 EMPTY                   
         DC    CL18'X   X   XO   O   O'           01-06 ONE FULL                
         DC    CL18' XXX XXX  OOO OOO '           07-12 TWO SAME FULL           
         DC    CL18' XOO XXO  OXX OOX '           13-18 TWO DIFF FULL           
         DC    CL18'XOOOXOOOXOXXXOXXXO'           19-24 ALL FULL DIFF           
         DC    CL06'XXXOOO'                       25-26 ALL FULL SAME           
         SPACE 1                                                                
PL1GOS35 DC    XL9'000325016721876385'                                          
         SPACE 1                                                                
PL2GOS35 DS    0CL140                                                           
         DC    XL5'6468464334'                    XO                            
         DC    XL5'683F8F3F8F'                    X O                           
         DC    XL5'2428414214'                    X  O                          
         DC    XL5'24264F6F4F'                    X    O                        
         DC    XL5'281F8F1F8F'                    X     O                       
         DC    XL5'62642F4F2F'                    X      O                      
         DC    XL5'2662266226'                    X       O                     
         DC    XL5'865F6F5F6F'                    O X                           
         DC    XL5'8486454854'                     OX                           
         DC    XL5'04088F4F8F'                      XO                          
         DC    XL5'4041040614'                      X  O                        
         DC    XL5'0880088008'                      X   O                       
         DC    XL5'84800F4F0F'                      X    O                      
         DC    XL5'061F6F1F6F'                      X     O                     
         DC    XL5'822F7F2F7F'                    O     X                       
         DC    XL5'04084F8F4F'                     O    X                       
         DC    XL5'0880088008'                      O   X                       
         DC    XL5'4748828474'                        O X                       
         DC    XL5'84804F0F4F'                         OX                       
         DC    XL5'4043020434'                          XO                      
         DC    XL5'022F3F2F3F'                          X O                     
         DC    XL5'2662266226'                    O       X                     
         DC    XL5'24264F6F4F'                     O      X                     
         DC    XL5'600F7F0F7F'                      O     X                     
         DC    XL5'62644F2F4F'                       O    X                     
         DC    XL5'4647606474'                         O  X                     
         DC    XL5'200F5F0F5F'                          O X                     
         DC    XL5'2024424554'                           OX                     
         SPACE 1                                                                
PL2BRDS  DS    0CL252                                                           
         DC    CL9'XO       '                                                   
         DC    CL9'X O      '                                                   
         DC    CL9'X  O     '                                                   
         DC    CL9'X    O   '                                                   
         DC    CL9'X     O  '                                                   
         DC    CL9'X      O '                                                   
         DC    CL9'X       O'                                                   
         DC    CL9'O X      '                                                   
         DC    CL9' OX      '                                                   
         DC    CL9'  XO     '                                                   
         DC    CL9'  X  O   '                                                   
         DC    CL9'  X   O  '                                                   
         DC    CL9'  X    O '                                                   
         DC    CL9'  X     O'                                                   
         DC    CL9'O     X  '                                                   
         DC    CL9' O    X  '                                                   
         DC    CL9'  O   X  '                                                   
         DC    CL9'   O  X  '                                                   
         DC    CL9'     OX  '                                                   
         DC    CL9'      XO '                                                   
         DC    CL9'      X O'                                                   
         DC    CL9'O       X'                                                   
         DC    CL9' O      X'                                                   
         DC    CL9'  O     X'                                                   
         DC    CL9'   O    X'                                                   
         DC    CL9'     O  X'                                                   
         DC    CL9'      O X'                                                   
         DC    CL9'       OX'                                                   
         SPACE 1                                                                
OXDP1F2  DC    XL6'010200020001'                                                
         SPACE 1                                                                
BLKBRD   DC    CL9'         '                                                   
LBLBRD   DC    CL9'ABCDEFGHI'                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
OXODATA  DSECT                                                                  
         DS    0F                                                               
OXDINCHR DS    CL1                                INPUT CHR FROM PLAYER         
OXDWHOGO DS    CL1                                CONTAINS O OR X               
         DS    CL1                                                              
OXDBRD   DS    CL9                                BOARD SQS 0 THRU 8            
OXDRCD   DS    8CL3                               ROWS,COLS,&DGLS ALPHA         
OXDRCDV  DS    8XL1                               ROWS,COLS,&DGLS BINRY         
OXDSELRV DS    8XL1                                                             
OXDSELRN DS    8XL1                                                             
OXDHF    DS    H                                  HIS FIRST GO                  
OXDMF    DS    H                                  MY FIRST GO                   
OXDCNT   DS    H                                  COUNT OF # OF GOES            
OXDSQR   DS    H                                  CURRENT SQ NUMB               
OXDPLAN  DS    H                                  PLAN #                        
OXDPGO5  DS    H                                  PLANED GO #5                  
OXDTIM   DS    D                                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
PLIST    DS    6F                                                               
SAVER1   DS    F                                                                
AGETFACT DS    A                                                                
AOXOGAME DS    A                                                                
LANG     DS    X                                                                
WORKX    EQU   *                                                                
         PRINT OFF                                                              
* DDFLDIND                                                                      
       ++INCLUDE DDFLDIND                                                       
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
GAOXOFFD DSECT                                                                  
         DS    CL64                                                             
* GAOXOFFD                                                                      
       ++INCLUDE GAOXOFFD                                                       
         SPACE 1                                                                
MSCORE   DS    F                   SAVE STORAGE AT END OF TWA                   
YSCORE   DS    F                                                                
PARA     DS    F                                                                
OLDLA    DS    CL1                                                              
OLDLB    DS    CL1                                                              
NEWLA    DS    CL1                                                              
NEWLB    DS    CL1                                                              
TURN     DS    CL1                                                              
OLDBOARD DS    CL9                                                              
*                                                                               
OXOSAVE  DS    0F                  SAVE STORAGE FOR OXOGAME                     
LETTER   DS    CL1                                                              
         DS    CL2                                                              
NEWBOARD DS    CL9                                                              
         DS    CL80                                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GAOXO00   05/01/02'                                      
         END                                                                    

*          DATA SET CPINQ03    AT LEVEL 003 AS OF 09/01/00                      
*PHASE TC0303A                                                                  
         TITLE 'COST PER POINT INQUIRY PROGRAM - GUIDE PHASE'                   
TC0303   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 4,**CPQG**                                                       
         LR    R7,RC                                                            
         USING LOCAL,R7                                                         
         L     RC,0(R1)                                                         
         USING GWS,RC              RC = GLOBAL W/S                              
         USING TC03TWA,RA          RA = TWA                                     
         SPACE 1                                                                
         CLI   NEXT,0              CONTINUATION SCREEN REQUESTED                
         BNE   G19                                                              
         EJECT                                                                  
*                  GENERATE TABLE TO DRIVE MAIN PROCESSING LOOP                 
         SPACE 3                                                                
G0       LA    R9,MAINTAB                                                       
         USING MAINTABD,R9         R9 = MAINTAB POINTER                         
         CLI   SAVEMENU,0                                                       
         BNE   G2                                                               
         SPACE 1                                                                
         MVC   HEADING1(6),=C'DPT-LN'                                           
         MVC   HEADING1+20(7),SAVEDNAM  NO MENU                                 
         CLI   SAVEDNAM+7,0                                                     
         BE    G1                                                               
         MVI   NOHEDLNS,2                                                       
         MVC   HEADING2(6),=6CL1'-'                                             
         MVC   HEADING2+20(7),SAVEDNAM+8                                        
G1       MVI   MAINID,1                 SINGLE TABLE ENTRY                      
         MVC   MAINMIN(2),SAVEKEY+9                                             
         MVI   MAINSCRC,22                                                      
         MVI   MAINSCRL,5                                                       
         MVI   MAINID+L'MAINTAB,X'FF'                                           
         B     G4                                                               
         SPACE 1                                                                
G2       ZIC   R3,SAVEMENU              MENU                                    
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         L     R3,MENUNDEX(R3)          INDEX OF MENUTAB NTRY ADDRESSES         
         AR    R3,RB                                                            
         USING MENUTABD,R3                                                      
         MVC   HEADING1,MENUHED1   HEADINGS                                     
         CLI   MENUHDLS,1                                                       
         BE    G3                                                               
         MVC   HEADING2,MENUHED2                                                
         MVI   NOHEDLNS,2                                                       
G3       MVC   MENU,MENUKEYS       STRING OF MENU KEYS                          
         MVC   SAVEKEY+9(2),MENU                                                
         ZIC   R4,MENUSIZE                                                      
G3A      MVC   MAINID(3),MENUID                                                 
         MVC   MAINSCRC(2),MENUSCRC                                             
         LA    R9,L'MAINTAB(R9)                                                 
         LA    R3,L'MENUMENU(R3)                                                
         BCT   R4,G3A                                                           
         MVI   MAINID,X'FF'                                                     
         EJECT                                                                  
*                  MAIN PROCESSING LOOP TO UPDATE MATRIX                        
         SPACE 3                                                                
G4       LA    R6,SCANBLK          R6 = A(MATRIX ENTRY BEING BUILT)             
         XC    SCANBLK(L'MATRIX),SCANBLK                                        
         USING MATRIXD,R6                                                       
         SPACE 1                                                                
G5       MVC   KEY,SAVEKEY                                                      
G6       GOTO1 AHIGH               READ A RECORD                                
         CLC   SAVEKEY(11),IO      CHECK KEY                                    
         BE    G7                                                               
         CLI   SAVEMENU,0          IF IT DOESNT MATCH HAVE WE A MENU ?          
         BE    G17                                                              
         SPACE 1                                                                
         LA    R7,2(R7)            HAVE WE REACHED THE END OF IT ?              
         CLI   0(R7),X'FF'                                                      
         BE    G17                                                              
         MVC   SAVEKEY+9(2),0(R7)  IF NOT UPDATE SAVEKEY AND READ ANTHR         
         XC    SAVEKEY+11(5),SAVEKEY+11                                         
         B     G5                                                               
         SPACE 3                                                                
G7       LA    R4,IO               COME HERE WITH A KEY THAT FITS               
         USING CPKEYD,R4                                                        
         LA    R5,CPRECORD         R5 = ELEMENT POINTER                         
         USING CPPERFD,R5                                                       
         GOTO1 AFILTER                                                          
         BZ    G16                 RECORD ELIMINATED BY FILTERS                 
         SR    R0,R0                                                            
         SPACE 2                                                                
G8       CLI   0(R5),0             LOOK FOR A PERFORMANCE ELEMENT               
         BE    G16                 WITHIN DATE RANGE                            
         CLC   ENDDAT,CPPYEAR                                                   
         BL    G16                                                              
         CLC   STARTDAT,CPPYEAR                                                 
         BH    G15                                                              
         LA    R9,MAINTAB                                                       
         SPACE 1                                                                
G9       CLI   0(R9),X'FF'         LOOK FOR A MAINTAB ENTRY THAT                
         BE    G15                 MATCHES ON DEMOS                             
         CLC   CPKTARGT(2),MAINMIN                                              
         BNE   G14                                                              
         SPACE 1                                                                
         CLC   CPKDAYPT(2),MATDAYPT DOES LAST MATRIX ENTRY MATCH ON             
         BNE   G10                 DAY-PART/LENGTH/ID                           
         CLC   MAINID,MATID                                                     
         BE    G13                                                              
         SPACE 1                                                                
G10      LA    R6,SCANBLK          IF NOT BUILD ONE IN SCANBLK WITH             
         MVI   FIRST,1             KEY AND ZERO VALUES                          
         MVC   MATDAYPT(2),CPKDAYPT                                             
         MVC   MATID,MAINID                                                     
         XC    MATVALA(8),MATVALA                                               
         GOTO1 ASEARCH             ADD IT TO, OR FIND IT IN, THE MATRIX         
         BZ    G17                 MATRIX FULL                                  
         L     R6,DMCB             R6 = A(NEW OR OLD MATRIX ENTRY)              
         SPACE 1                                                                
G13      L     RF,AMATRXSR         CALL MATRIX UPDATE SUBROUTINE FOR            
         GOTO1 (RF)                THIS DATA TYPE                               
         B     G15                                                              
         SPACE 1                                                                
G14      LA    R9,L'MAINTAB(R9)    MAINTAB BUMP                                 
         B     G9                                                               
         SPACE 1                                                                
G15      IC    R0,CPPLEN           ELEMENT BUMP                                 
         AR    R5,R0                                                            
         B     G8                                                               
         SPACE 3                                                                
G16      MVC   KEY,CPKEY           RECORD BUMP (ADD 1 TO LAST WORD OF           
         L     R1,KEY+12           KEY)                                         
         AH    R1,=H'1'                                                         
         ST    R1,KEY+12                                                        
         B     G6                                                               
         SPACE 3                                                                
G17      LA    R3,L'MATRIX         TERMINATE THE MATRIX                         
         M     R2,RECNUM                                                        
         LA    R3,MATRIX(R3)                                                    
         MVI   0(R3),X'FF'                                                      
         ST    R3,AENDMTRX         STORE A(END OF MATRIX)                       
         B     G18                                                              
         EJECT                                                                  
*                  SET UP DISPLAY SCREEN FROM MATRIX VIA MAINTAB                
         SPACE 3                                                                
G18      CLI   FIRST,0                                                          
         BNE   G22                                                              
         MVI   FERN,MINE                                                        
         MVC   CPQHEAD(L'NODSPLAY),NODSPLAY                                     
         B     GEND                                                             
NODSPLAY DC    CL50'NOTHING TO DISPLAY - ENTER FIELDS FOR NEXT INQUIRY'         
         SPACE 2                                                                
G19      GOTO1 ARDIR               IF CONTINUATION SCREEN, RESTORE              
G20      L     R1,ATIA                                                          
         USING TC03SAVX,R1                                                      
         LA    R2,SAVEMTRX                                                      
         LA    R4,L'MATRIX                                                      
         LH    R5,SAVEMXLN                                                      
         LA    R5,0(R2,R5)                                                      
         SR    R5,R4                                                            
         LA    R6,MATRIX-L'MATRIX                                               
         SPACE 1                                                                
G21      LA    R6,L'MATRIX(R6)                                                  
         MVC   0(L'MATRIX,R6),0(R2)                                             
         BXLE  R2,R4,G21                                                        
         ST    R6,AENDMTRX                                                      
         MVC   MAINTAB(241),SAVETAB                                             
         MVI   NEXT,0                                                           
         SPACE 3                                                                
G22      GOTO1 AFORMAT             SET UP DISPLAY                               
GEND     XMOD1 1                                                                
         EJECT                                                                  
*                  MENU DEFINITIONS - COVERED BY DSECT MENUTABD                 
         SPACE 3                                                                
MENUNDEX DS    0F                  INDEX TO MENU DEFINITIONS                    
*&&UK                                                                           
         DC    A(UKGROUP)                                                       
*&&                                                                             
*&&US                                                                           
         DC    A(LEADING)                                                       
         DC    A(WOMEN)                                                         
         DC    A(MEN)                                                           
         DC    A(VIEWERS)                                                       
         DC    A(YOUNG)                                                         
         DC    A(ADULTS)                                                        
         DC    A(CHILD)                                                         
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*&&                                                                             
         SPACE 3                                                                
*&&UK                                                                           
UKGROUP  DC    AL1(1)              UK GROUP                                     
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN  HOMES  -----------VIEWERS------'                    
         DC    CL39'-----  -MEN-  WOMEN  ----HOUSEWIVES----'                    
         DC    CL39'------  -----    ALL   -45   +45 ADULT '                    
         DC    CL39'CHILD  -----  -----  +CHILD   -45   ALL'                    
         DC    AL1(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,255)         
         DS    2C                                                               
         DC    AL1(11)                                                          
         SPACE 2                                                                
         DC    AL1(01,01,01,09,5)  ID/TARGET-DEMO/SUB-DEMO/STRT.COL/LEN         
         DC    AL1(02,04,04,17,5)                                               
         DC    AL1(03,06,06,23,5)                                               
         DC    AL1(04,07,07,29,5)                                               
         DC    AL1(05,03,03,35,5)                                               
         DC    AL1(06,05,05,41,5)                                               
         DC    AL1(07,08,08,48,5)                                               
         DC    AL1(08,09,09,55,5)                                               
         DC    AL1(09,11,11,63,5)                                               
         DC    AL1(10,10,10,69,5)                                               
         DC    AL1(11,02,02,75,5)                                               
*&&                                                                             
*&&US                                                                           
LEADING  DC    AL1(1)                                                           
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN HOMES ---------------WOMEN------'                    
         DC    CL39'--------- ----MEN---- -----ADULTS------'                    
         DC    CL39'------ ----- TOTAL 18-34 18-49 35-49   '                    
         DC    CL39'35+   50+ 18-34 25-49 TOTAL 18-34 18-49'                    
         DC    AL1(48,48,52,52,64,64,78,78,79,79,80,80,84,84,85,85)             
         DC    AL1(86,86,116,116,117,117,118,118,255)                           
         DC    AL1(12)                                                          
         SPACE 2                                                                
         DC    AL1(01,064,064,09,5)                                             
         DC    AL1(02,080,080,15,5)                                             
         DC    AL1(03,078,078,21,5)                                             
         DC    AL1(04,079,079,27,5)                                             
         DC    AL1(05,084,084,33,5)                                             
         DC    AL1(06,085,085,39,5)                                             
         DC    AL1(07,086,086,45,5)                                             
         DC    AL1(08,048,048,51,5)                                             
         DC    AL1(09,052,052,57,5)                                             
         DC    AL1(10,118,118,63,5)                                             
         DC    AL1(11,116,116,69,5)                                             
         DC    AL1(12,117,117,75,5)                                             
         SPACE 3                                                                
WOMEN    DC    AL1(2)                                                           
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN --------------------------------'                    
         DC    CL39'-WOMEN---------------------------------'                    
         DC    CL39'------ 18-24 18-34 18-49   18+ 25-34 25'                    
         DC    CL39'-49 25-54 25-64   25+ 35-49   35+  WORK'                    
         DC    AL1(77,77,78,78,79,79,80,80,81,81,82,82,87,87,88,88)             
         DC    AL1(83,83,84,84,85,85,97,97,255)                                 
         DC    AL1(12)                                                          
         SPACE 1                                                                
         DC    AL1(01,77,77,09,5)                                               
         DC    AL1(02,78,78,15,5)                                               
         DC    AL1(03,79,79,21,5)                                               
         DC    AL1(04,80,80,27,5)                                               
         DC    AL1(05,81,81,33,5)                                               
         DC    AL1(06,82,82,39,5)                                               
         DC    AL1(07,87,87,45,5)                                               
         DC    AL1(08,88,88,51,5)                                               
         DC    AL1(09,83,83,57,5)                                               
         DC    AL1(10,84,84,63,5)                                               
         DC    AL1(11,85,85,69,5)                                               
         DC    AL1(12,97,97,75,5)                                               
         SPACE 3                                                                
MEN      DC    AL1(3)                                                           
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN --------------------------------'                    
         DC    CL39'--MEN----------------------------------'                    
         DC    CL39'------ 18-24 18-34 18-49   18+ 25-34 25'                    
         DC    CL39'-49 25-54 25-64   25+ 35-49   35+   50+'                    
         DC    AL1(47,47,48,48,49,49,50,50,51,51,52,52,57,57,58,58)             
         DC    AL1(53,53,54,54,55,55,56,56,255)                                 
         DC    AL1(12)                                                          
         SPACE 1                                                                
         DC    AL1(01,47,47,09,5)                                               
         DC    AL1(02,48,48,15,5)                                               
         DC    AL1(03,49,49,21,5)                                               
         DC    AL1(04,50,50,27,5)                                               
         DC    AL1(05,51,51,33,5)                                               
         DC    AL1(06,52,52,39,5)                                               
         DC    AL1(07,57,57,45,5)                                               
         DC    AL1(08,58,58,51,5)                                               
         DC    AL1(09,53,53,57,5)                                               
         DC    AL1(10,54,54,63,5)                                               
         DC    AL1(11,55,55,69,5)                                               
         DC    AL1(12,56,56,75,5)                                               
         SPACE 3                                                                
VIEWERS  DC    AL1(4)                                                           
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN --------------------------------'                    
         DC    CL39'VIEWERS--------------------------------'                    
         DC    CL39'------ 12-24 12-34 12-49 18-34 18-49   '                    
         DC    CL39'18+ 25-34 25-49 25-54 35-49   35+   50+'                    
         DC    AL1(110,110,111,111,112,112,116,116,117,117,118,118)             
         DC    AL1(119,119,120,120,121,121,123,123,124,124,125,125,255)         
         DC    AL1(12)                                                          
         SPACE 1                                                                
         DC    AL1(01,110,110,09,5)                                             
         DC    AL1(02,111,111,15,5)                                             
         DC    AL1(03,112,112,21,5)                                             
         DC    AL1(04,116,116,27,5)                                             
         DC    AL1(05,117,117,33,5)                                             
         DC    AL1(06,118,118,39,5)                                             
         DC    AL1(07,119,119,45,5)                                             
         DC    AL1(08,120,120,51,5)                                             
         DC    AL1(09,121,121,57,5)                                             
         DC    AL1(10,123,123,63,5)                                             
         DC    AL1(11,124,124,69,5)                                             
         DC    AL1(12,125,125,75,5)                                             
         SPACE 3                                                                
YOUNG    DC    AL1(5)                                                           
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN     --------------------CHILDREN'                    
         DC    CL39'-----------------------     ---TEENS---'                    
         DC    CL39'------     2-5  2-11  2-17    2+  6-11 '                    
         DC    CL39' 6-17    6+ 12-17 12-24     BOYS  GIRLS'                    
         DC    AL1(98,98,99,99,104,104,105,105,100,100,108,108)                 
         DC    AL1(109,109,102,102,110,110,43,43,67,67,255)                     
         DS    2C                                                               
         DC    AL1(11)                                                          
         SPACE 1                                                                
         DC    AL1(01,098,098,11,5)                                             
         DC    AL1(02,099,099,17,5)                                             
         DC    AL1(03,104,104,23,5)                                             
         DC    AL1(04,105,105,29,5)                                             
         DC    AL1(05,100,100,35,5)                                             
         DC    AL1(06,108,108,41,5)                                             
         DC    AL1(07,109,109,47,5)                                             
         DC    AL1(08,102,102,53,5)                                             
         DC    AL1(09,110,110,59,5)                                             
         DC    AL1(10,043,043,68,5)                                             
         DC    AL1(11,067,067,75,5)                                             
         SPACE 3                                                                
ADULTS   DC    AL1(6)                                                           
         SPACE 3                                                                
CHILD    DC    AL1(7)                                                           
         DC    AL1(2)                                                           
         DC    CL39'DPT-LN                  ---------------'                    
         DC    CL39'CHILDREN----------------               '                    
         DC    CL39'------                  2-5            '                    
         DC    CL39'  2-11              6-11               '                    
         DC    AL1(98,98,99,99,100,100,255)                                     
         DS    18C                                                              
         DC    AL1(3)                                                           
         SPACE 1                                                                
         DC    AL1(1,098,098,24,5)                                              
         DC    AL1(2,099,099,42,5)                                              
         DC    AL1(3,100,100,60,5)                                              
         SPACE 3                                                                
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
LOCAL    DSECT                                                                  
MENU     DS    CL25      X         UP TO 12X2-BYTE DEMO KEYS                    
         EJECT                                                                  
*                  NESTED INCLUDES FOR CPINQDSECT & CPGENFILE                   
         PRINT OFF                                                              
       ++INCLUDE CPINQDSECT                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CPINQ03   09/01/00'                                      
         END                                                                    

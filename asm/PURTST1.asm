*          DATA SET PURTST1    AT LEVEL 087 AS OF 10/18/84                      
*PHASE PURTST1,*                                                                
          PRINT      ON,NOGEN                                                   
           MACRO                                                                
&NAME      TAB2      &SIZE,&COLOR,&$                                            
           LCLA      &A                                                         
&NAME      DS        0F                                                         
$B&SYSNDX  DC        AL1($$&SIZE)                                               
           DC        AL1($$&COLOR)                                              
           DC        AL2($E&SYSNDX-$B&SYSNDX)                                   
&A         SETA      1                                                          
.P2        AIF       (T'&$(&A) EQ 'O').P1                                       
           DC        F'&$(&A)'                                                  
&A         SETA      &A+1                                                       
           AGO       .P2                                                        
.P1        ANOP                                                                 
$E&SYSNDX  EQU       *                                                          
           MEND                                                                 
          MACRO                                                                 
&NAME     SUITM     &PUB,&BRAND,&EDITN,&ISSUE,&TYPE,&COLOR,&COPY,      X        
               &GRP=0,&SEQ=0,&BLD=N,&POS=N                                      
          GBLC      &MPUB,&MBRAND,&MEDITN,&MISSUE                               
          LCLA      &N                                                          
&NAME     DS        0F                                                          
          AIF       (T'&PUB EQ 'O').P1                                          
&MPUB     SETC      '&PUB'                                                      
.P1       DC        CL4'&MPUB'                                                  
          AIF       (T'&BRAND EQ 'O').P2                                        
&MBRAND   SETC      '&BRAND'                                                    
.P2       DC        CL3'&MBRAND'                                                
          AIF       (T'&EDITN EQ 'O').P3                                        
&MEDITN   SETC      '&EDITN'                                                    
.P3       DC        CL2'&MEDITN'                                                
          AIF       (T'&ISSUE EQ 'O').P4                                        
&MISSUE   SETC      '&ISSUE'                                                    
.P4       DC        XL3'&MISSUE'                                                
          DC        XL1'&GRP'                                                   
          DC        XL1'&SEQ'                                                   
          DC        AL1($$&TYPE)                                                
          DC        AL1($$&COLOR)                                               
          DC        CL6'&COPY'                                                  
&N        SETA      0                                                           
          AIF       ('&BLD' EQ 'N').P5                                          
&N        SETA      &N+1                                                        
.P5       AIF       ('&POS' EQ 'N').P6                                          
&N        SETA      &N+2                                                        
.P6       DC        H'&N'                                                       
          DC        F'0'                                                        
          DC        F'0'                                                        
          DC        F'0'                                                        
          DC        F'0'                                                        
          DC        F'0'                                                        
          DC        F'0'                                                        
          MEND                                                                  
       ++INCLUDE PURTBASE                                                       
FIRST      CSECT                                                                
GO         NBASE     ,PURTST1,WORKA                                             
           LA        R9,FBLD                                                    
           USING     FBLD,R9                                                    
           OPEN      (DUMP,OUTPUT,PRINT,OUTPUT)                                 
           LA        RA,WORK                                                    
           USING     $WORK,RA                                                   
           PRINT     ON,NOGEN                                                   
           EJECT                                                                
***********************************************************************         
*                                                                     *         
*                    MACROS TO RATE SCHEDULE START HERE               *         
*                                                                     *         
***********************************************************************         
***   ULTRA                                                                     
           LA        RW1,ULTRF                                                  
           LA        RW2,ULTRL                                                  
           STM       RW1,RW2,$IF                                                
           ITEMS                                                                
             CTIN    1                                                          
             LOOP                                                               
           LUBT      ULTRA1A                                                    
           ITEMS                                                                
             GBRB    ULTRA1B                                                    
             GBRA                                                               
             PFPCT   10,FBLD,CODE=01                                            
             PFPCT   15,FPOSP,CODE=02                                           
             LOOP                                                               
           EJECT                                                                
***   BUSINESS WEEK                                                             
           LA        RW1,BUSWF                                                  
           LA        RW2,BUSWL                                                  
           STM       RW1,RW2,$IF                                                
           ITEMS     BWFANA                                                     
             CTIN    2                                                          
             LOOP                                                               
           LUBT      BWTABA                                                     
           ITEMS     BWFANA                                                     
             GBRB    BWTABWW,BWFWW                                              
             GBRB    BWTABNA,BWFNA                                              
             GBRB    BWTABNAA,BWFNAA                                            
             GBRB    BWTABNAE,BWFNAE                                            
             GBRB    BWTABNAL,BWFNAL                                            
             LOOP                                                               
           ITEMS     BWFAXI                                                     
             CTIN    2                                                          
             LOOP                                                               
           LUBT      BWTABA                                                     
           ITEMS     BWFAXI                                                     
             GBRB    BWTABI,BWFI                                                
             GBRB    BWTABII,BWFII                                              
             GBRB    BWTABIA,BWFIA                                              
             GBRB    BWTABIE,BWFIE                                              
             GBRB    BWTABIL,BWFIL                                              
             LOOP                                                               
           ITEMS     BWFAXNA                                                    
             CTIN    2                                                          
             LOOP                                                               
           LUBT      BWTABA                                                     
           ITEMS     BWFAXNA                                                    
             GBRB    BWTABIN,BWFIN                                              
             GBRB    BWTABINA,BWFINA                                            
             GBRB    BWTABINE,BWFINE                                            
             GBRB    BWTABINL,BWFINL                                            
             LOOP                                                               
           CPDGRP                                                               
             ITEMS                                                              
               CTIN  1                                                          
               LOOP                                                             
             LUBT    BWTABB                                                     
             ITEMS                                                              
               PVPCT CODE=03                                                    
               LOOP                                                             
             LOOP                                                               
           ITEMS                                                                
             PFPCT   15,FBLD,CODE=01                                            
             PFPCT   10,FPOSP,CODE=02                                           
             GBRA                                                               
             LOOP                                                               
***********************************************************************         
*                                                                     *         
*                    MACROS END HERE                                  *         
*                                                                     *         
***********************************************************************         
           PRINT     OFF                                                        
           LA        RW1,ULTRF                                                  
           LA        RW2,BUSWL                                                  
           STM       RW1,RW2,$IF                                                
           MVI       PTLN,C' '                                                  
           MVC       PTLN+1(120),PTLN                                           
           MVI       PTLN,C'1'                                                  
           PUT       PRINT,PTLN                                                 
           ITEMS                                                                
           MVI       PTLN,C' '                                                  
           MVC       PTLN+1(120),PTLN                                           
           MVC       PTLN+1(4),$PUB                                             
           MVC       PTLN+6(3),$BRAND                                           
           MVC       PTLN+10(2),$EDITN                                          
           UNPK      WRKU(11),$ISSUE(6)                                         
           MVC       PTLN+13(6),WRKU                                            
           MVC       PTLN+20(2),WRKU+6                                          
           MVC       PTLN+23(2),WRKU+8                                          
           SR        RW1,RW1                                                    
           IC        RW1,$TYPE                                                  
           AR        RW1,RW1                                                    
           AR        RW1,RW1                                                    
           AR        RW1,RW1                                                    
           LA        RW1,TTAB1(RW1)                                             
           MVC       PTLN+26(8),0(RW1)                                          
           SR        RW1,RW1                                                    
           IC        RW1,$COLOR                                                 
           AR        RW1,RW1                                                    
           AR        RW1,RW1                                                    
           LA        RW1,TTAB2(RW1)                                             
           MVC       PTLN+35(4),0(RW1)                                          
           MVC       PTLN+40(6),$COPY                                           
           UNPK      WRKU(5),$FLAGS(3)                                          
           MVC       PTLN+47(4),WRKU                                            
           L         RW1,$OPENRT                                                
           CVD       RW1,WRKD                                                   
           UNPK      WRKU(15),WRKD                                              
           LTR       RW1,RW1                                                    
           BNL       *+8                                                        
           MVI       PTLN+52,C'-'                                               
           MVC       PTLN+53(8),WRKU+7                                          
           MVZ       PTLN+60(1),PTLN+59                                         
           L         RW1,$EARNRT                                                
           CVD       RW1,WRKD                                                   
           UNPK      WRKU(15),WRKD                                              
           LTR       RW1,RW1                                                    
           BNL       *+8                                                        
           MVI       PTLN+62,C'-'                                               
           MVC       PTLN+63(8),WRKU+7                                          
           MVZ       PTLN+70(1),PTLN+69                                         
           L         RW1,$ACCPCT                                                
           CVD       RW1,WRKD                                                   
           UNPK      WRKU(15),WRKD                                              
           LTR       RW1,RW1                                                    
           BNL       *+8                                                        
           MVI       PTLN+72,C'-'                                               
           MVC       PTLN+73(2),WRKU+13                                         
           MVZ       PTLN+74(1),PTLN+73                                         
           L         RW1,$CURPCT                                                
           CVD       RW1,WRKD                                                   
           UNPK      WRKU(15),WRKD                                              
           LTR       RW1,RW1                                                    
           BNL       *+8                                                        
           MVI       PTLN+76,C'-'                                               
           MVC       PTLN+77(2),WRKU+13                                         
           MVZ       PTLN+78(1),PTLN+77                                         
           UNPK      WRKU(3),$APPDIS(2)                                         
           UNPK      WRKU+2(15),$APPDIS+1(8)                                    
           MVC       PTLN+80(16),WRKU                                           
           PUT       PRINT,PTLN                                                 
           LOOP                                                                 
           MVI       PTLN,C' '                                                  
           MVC       PTLN+1(120),PTLN                                           
           MVI       PTLN,C'1'                                                  
           PUT       PRINT,PTLN                                                 
           CLOSE     (DUMP,,PRINT)                                              
           XBASE                                                                
           PRINT     ON                                                         
BWFANA     CLI       $EDITN,C'N'                                                
           BE        4(RRFF)                                                    
           BR        RRFF                                                       
BWFAXI     CLI       $EDITN,C'O'                                                
           BNE       4(RRFF)                                                    
           BR        RRFF                                                       
BWFAXNA    CLI       $EDITN,C'N'                                                
           BNE       4(RRFF)                                                    
           BR        RRFF                                                       
BWFWW      CLI       $EDITN+1,C'O'                                              
           BE        BWFANA                                                     
           BR        RRFF                                                       
BWFNA      CLI       $EDITN+1,C' '                                              
           BE        BWFANA                                                     
           BR        RRFF                                                       
BWFNAA     CLI       $EDITN+1,C'A'                                              
           BE        BWFANA                                                     
           BR        RRFF                                                       
BWFNAE     CLI       $EDITN+1,C'E'                                              
           BE        BWFANA                                                     
           BR        RRFF                                                       
BWFNAL     CLI       $EDITN+1,C'L'                                              
           BE        BWFANA                                                     
           BR        RRFF                                                       
BWFI       CLI       $EDITN+1,C' '                                              
           BE        BWFI1                                                      
           BR        RRFF                                                       
BWFI1      CLI       $EDITN,C'I'                                                
           BE        4(RRFF)                                                    
           BR        RRFF                                                       
BWFII      CLI       $EDITN+1,C'O'                                              
           BE        BWFI1                                                      
           BR        RRFF                                                       
BWFIA      CLI       $EDITN+1,C'A'                                              
           BE        BWFI1                                                      
           BR        RRFF                                                       
BWFIE      CLI       $EDITN+1,C'E'                                              
           BE        BWFI1                                                      
           BR        RRFF                                                       
BWFIL      CLI       $EDITN+1,C'L'                                              
           BE        BWFI1                                                      
           BR        RRFF                                                       
BWFIN1     CLI       $EDITN,C'O'                                                
           BE        4(RRFF)                                                    
           BR        RRFF                                                       
BWFIN      CLI       $EDITN+1,C' '                                              
           BE        BWFIN1                                                     
           BR        RRFF                                                       
BWFINA     CLI       $EDITN+1,C'A'                                              
           BE        BWFIN1                                                     
           BR        RRFF                                                       
BWFINE     CLI       $EDITN+1,C'E'                                              
           BE        BWFIN1                                                     
           BR        RRFF                                                       
BWFINL     CLI       $EDITN+1,C'L'                                              
           BE        BWFIN1                                                     
           BR        RRFF                                                       
FBLD       TM        $FLAGS+1,1                                                 
           BO        4(RRFF)                                                    
           BR        RRFF                                                       
FPOSP      TM        $FLAGS+1,2                                                 
           BO        4(RRFF)                                                    
           BR        RRFF                                                       
           PRINT     OFF                                                        
PTLN       DS        CL121                                                      
TTAB1      DC        CL8'SPREAD'                                                
           DC        CL8'SPREAD'                                                
           DC        CL8'PAGE'                                                  
           DC        CL8'2/3 PG'                                                
           DC        CL8'1/2 PG'                                                
           DC        CL8'1/4 PG'                                                
           DC        CL8'1/8 PG'                                                
           DC        CL8'1/3 PG'                                                
           DC        CL8'1/6 PG'                                                
           DC        CL8'2ND CVR'                                               
           DC        CL8'3RD CVR'                                               
           DC        CL8'4TH CVR'                                               
TTAB2      DC        CL4'B/W'                                                   
           DC        CL4'1C'                                                    
           DC        CL4'2C'                                                    
           DC        CL4'3C'                                                    
           DC        CL4'4C'                                                    
           DC        CL4'5C'                                                    
WRKU       DS        CL17                                                       
WRKD       DS        D                                                          
WORKA      DS        20D                                                        
WORK       DS        10D                                                        
           PRINT     ON,NOGEN                                                   
FIRSTITM   EQU       *                                                          
ULTRF     SUITM     ULTR,TST,N,840901,PG,B#W,TEST1                              
          SUITM     ,,,,PG,4C,TEST2                                             
          SUITM     ,,,,PG,4C,TEST3                                             
ULTRL     SUITM     ,,,,PG,4C,TEST4,BLD=Y                                       
BUSWF     SUITM     BUSW,,N,840901,PG,B#W,T1                                    
          SUITM     ,,I,,1#2PG,B#W,T2A,BLD=Y                                    
          SUITM     ,,I,,1#2PG,B#W,T2B                                          
          SUITM     ,,I,,1#2PG,B#W,T2C                                          
          SUITM     ,,N,841001,PG,B#W,T3                                        
          SUITM     ,,O,,1#2PG,B#W,T4,BLD=Y,POS=Y                               
          SUITM     ,,N,841101,PG,4C,T5                                         
          SUITM     ,,I,,1#3PG,B#W,T6A,POS=Y                                    
          SUITM     ,,I,,1#3PG,B#W,T6B                                          
          SUITM     ,,I,,1#3PG,B#W,T6C                                          
          SUITM     ,,N,851201,PG,4C,T6,GRP=1,SEQ=1                             
          SUITM     ,,,,PG,4C,T7,GRP=1,SEQ=2,BLD=Y                              
          SUITM     ,,,,PG,4C,T8,GRP=1,SEQ=3                                    
BUSWL     SUITM     ,,,,1#6PG,B#W,T9,GRP=1,SEQ=4                                
BWTABA     DC        F'6'                                                       
           DC        F'1'                                                       
           DC        F'0'                                                       
           DC        F'7'                                                       
           DC        F'4'                                                       
           DC        F'13'                                                      
           DC        F'8'                                                       
           DC        F'26'                                                      
           DC        F'12'                                                      
           DC        F'39'                                                      
           DC        F'16'                                                      
           DC        F'52'                                                      
           DC        F'20'                                                      
BWTABB     DC        F'5'                     CPD DISCOUNTS                     
           DC        F'1'                                                       
           DC        F'0'                                                       
           DC        F'4'                                                       
           DC        F'10'                                                      
           DC        F'8'                                                       
           DC        F'15'                                                      
           DC        F'12'                                                      
           DC        F'20'                                                      
           DC        F'16'                                                      
           DC        F'25'                                                      
BWTABWW    TAB2      PG,B#W,(27636,25704,23988,23600,23212,22824)               
           DC        F'0'      *****NOT DONE*****                               
BWTABNA    TAB2      PG,B#W,(24120,22440,20940,20600,20260,19920)               
           TAB2      2#3PG,B#W,(16880,15700,14660,14420,14180,13940)            
           TAB2      1#2PG,B#W,(14480,13460,12560,12360,12160,11960)            
           TAB2      1#3PG,B#W,(8680,8080,7540,7420,7300,7180)                  
           TAB2      1#6PG,B#W,(4420,4100,3840,3760,3700,3640)                  
           TAB2      PG,1C,(30640,28500,26600,26160,25740,25300)                
           TAB2      2#3PG,1C,(21440,19960,18620,18320,18020,17720)             
           TAB2      1#2PG,1C,(18380,17100,15960,15700,15440,15180)             
           TAB2      1#3PG,1C,(11020,10260,9580,9420,9260,9100)                 
           TAB2      1#6PG,1C,(5600,5220,4860,4780,4700,4640)                   
           TAB2      PG,4C,(36660,34100,31820,31300,30800,30280)                
           TAB2      2#3PG,4C,(25660,23880,22280,21920,21560,21200)             
           TAB2      1#2PG,4C,(22000,20460,19100,18780,18480,18160)             
           TAB2      1#3PG,4C,(13200,12280,11460,11280,11080,10900)             
           TAB2      CV2,4C,(36660,34100,31820,31300,30800,30280)               
           TAB2      CV3,4C,(36660,34100,31820,31300,30800,30280)               
           TAB2      CV4,4C,(42700,39700,37060,36460,35860,35280)               
           DC        F'0'                                                       
BWTABNAA   TAB2      PG,B#W,(25512,23736,22152,21788,21424,21072)               
           TAB2      PG,1C,(32404,30144,28136,27672,27216,26757)                
           TAB2      PG,4C,(38772,36068,33656,33100,32576,32020)                
           DC        F'0'                                                       
BWTABNAE   TAB2      PG,B#W,(26232,24408,22776,22400,22036,21660)               
           DC        F'0'          ***NOT DONE***                               
BWTABNAL   TAB2      PG,B#W,(25200,23448,21876,21524,21172,20808)               
           DC        F'0'          ***NOT DONE***                               
BWTABI     TAB2      PG,B#W,(13380,12440,11620,11420,11240,11060)               
           TAB2      2#3PG,B#W,(9360,8700,8140,8000,7860,7740)                  
           TAB2      1#2PG,B#W,(8020,7460,6980,6840,6740,6620)                  
           TAB2      1#3PG,B#W,(5360,4980,4640,4580,4500,4420)                  
           DC        F'0'          ***NOT DONE***                               
BWTABII    EQU       *                        ***NOT DONE***                    
BWTABIA    EQU       *                        ***NOT DONE***                    
BWTABIE    EQU       *                        ***NOT DONE***                    
BWTABIL    EQU       *                        ***NOT DONE***                    
BWTABIN    TAB2      PG,B#W,(5860,5440,5080,5000,4920,4840)                     
           TAB2      2#3PG,B#W,(4100,3800,3560,3500,3440,3380)                  
           TAB2      1#2PG,B#W,(3520,3260,3040,3000,2960,2900)                  
           TAB2      1#3PG,B#W,(2340,2180,2040,2000,1960,1940)                  
           TAB2      1#6PG,B#W,(1280,1200,1120,1100,1080,1060)                  
           DC        F'0'         ***NOT DONE***                                
BWTABINA   EQU       *                        ***NOT DONE***                    
BWTABINE   EQU       *                        ***NOT DONE***                    
BWTABINL   TAB2      PG,B#W,(1800,1680,1560,1540,1520,1480)                     
           TAB2      PG,1C,(2280,2120,1980,1940,1920,1880)                      
           TAB2      PG,4C,(2740,2540,2380,2340,2300,2260)                      
           DC        F'0'                                                       
ULTRA1A    DC        F'4'                                                       
           DC        F'1'                                                       
           DC        F'0'                                                       
           DC        F'4'                                                       
           DC        F'4'                                                       
           DC        F'6'                                                       
           DC        F'8'                                                       
           DC        F'12'                                                      
           DC        F'12'                                                      
ULTRA1B    TAB2      PG,B#W,(3390,3295,3220,3050)                               
           TAB2      2#3PG,B#W,(2540,2465,2415,2285)                            
           TAB2      1#2PG,B#W,(2035,1975,1935,1835)                            
           TAB2      1#3PG,B#W,(1355,1315,1285,1220)                            
           TAB2      1#6PG,B#W,(850,825,805,765)                                
           TAB2      PG,4C,(4290,4195,4120,3950)                                
           TAB2      2#3PG,4C,(3440,3365,3315,3185)                             
           TAB2      1#2PG,4C,(2935,2875,2835,2735)                             
           TAB2      1#3PG,4C,(2255,2215,2185,2120)                             
           TAB2      CV2,4C,(5190,5095,5020,4850)                               
           TAB2      CV3,4C,(4990,4895,4820,4650)                               
           TAB2      CV4,4C,(5490,5395,5320,5150)                               
          DC        F'0'                                                        
           PRINT     OFF                                                        
DUMP      DCB       DSORG=PS,RECFM=VBA,MACRF=(W),BLKSIZE=1632,         X        
               LRECL=125,DDNAME=DUMP                                            
PRINT      DCB       BLKSIZE=1573,DDNAME=SYSPRINT,DSORG=PS,            X        
               LRECL=121,MACRF=(PM),RECFM=FBA                                   
          END       GO                                                          

*          DATA SET DESVIPEP   AT LEVEL 054 AS OF 08/23/00                      
*PHASE SVIPEPA                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
         PRINT NOGEN                                                            
*                                                                               
SVIPEP   TITLE 'CREATE NSI INDICES FOR PEPSI'                                   
SVIPEP   CSECT                                                                  
         NBASE 600,SVIPEP,=V(REGSAVE)                                           
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
SETDEM   DS    0C                                                               
         LA    R4,FEB                                                           
         ST    R4,CURRENT                                                       
*                                                                               
SETHDR   MVI   CARDD,C' '                                                       
         MVC   CARDD+1(79),CARDD                                                
         MVC   CMKT,=C'0419'       MKT#                                         
         CLI   MARKET,1                                                         
         BNE   *+10                                                             
         MVC   CMKT,=C'0420'                                                    
*                                                                               
         MVC   CMED(3),=C'TNP'     MEDIA/SERVICE/SOURCE                         
*                                                                               
         ZIC   R5,MARKET                                                        
         ZIC   R6,DEMO                                                          
         MH    R5,=H'8'                                                         
         MH    R6,=H'2'                                                         
         LA    R5,8(R4,R5)                                                      
         AR    R5,R6                                                            
         MVC   CDAY,0(R4)          DAY...FROM DYTMS                             
*                                                                               
         LA    R7,4(R4)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R7)),CSTQH                                    
         SR    RE,RE                                                            
         IC    RE,CSTQH                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CSTQH,DUB           START QH                                     
*                                                                               
         LA    R7,6(R4)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R7)),CENDQH                                   
         SR    RE,RE                                                            
         IC    RE,CENDQH                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CENDQH,DUB          END QH                                       
*                                                                               
         ZIC   RE,DEMO                                                          
         LA    RF,DEMOTAB(RE)                                                   
         ZIC   RE,0(RF)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CAUD(2),DUB+6(2)                                                 
         MVC   CACT,=C'A'          ACTION CODE                                  
*                                                                               
         MVC   HALF,48(R5)         SEED NOV PUT                                 
         LH    R1,0(R5)            GET FEB DEMO                                 
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         LH    RE,HALF                                                          
         DR    R0,RE                                                            
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DEM(3),DUB+6(2)                                                  
         LA    R9,CMONS                                                         
         MVC   1(3,R9),DEM                                                      
         MVC   4(8,R9),0(R9)                                                    
         LA    R5,16(R5)                                                        
         LA    R9,12(R9)                                                        
*                                                                               
         LH    R1,0(R5)            GET MAY DEMO                                 
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         LH    RE,HALF                                                          
         DR    R0,RE                                                            
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DEM(3),DUB+6(2)                                                  
         MVC   1(3,R9),DEM                                                      
         MVC   4(8,R9),0(R9)                                                    
         LA    R5,16(R5)                                                        
         LA    R9,12(R9)                                                        
*                                                                               
         LH    R1,0(R5)            GET MAY DEMO                                 
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         LH    RE,HALF                                                          
         DR    R0,RE                                                            
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DEM(3),DUB+6(2)                                                  
         MVC   1(3,R9),DEM                                                      
         MVC   4(8,R9),0(R9)                                                    
         LA    R5,16(R5)                                                        
         LA    R9,12(R9)                                                        
*                                                                               
         LH    R1,0(R5)            GET MAY DEMO                                 
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         LH    RE,HALF                                                          
         DR    R0,RE                                                            
         A     R1,=F'1'                                                         
         SRL   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DEM(3),DUB+6(2)                                                  
         MVC   1(3,R9),DEM                                                      
         MVC   4(8,R9),0(R9)                                                    
         LA    R5,16(R5)                                                        
         LA    R9,12(R9)                                                        
*                                                                               
         L     R3,=A(OUT)          OUTPUT CARDD RECORD                          
         PUT   (R3),CARDD                                                       
*                                                                               
         XC    P,P                                                              
         MVC   P(80),CARDD         PRINT OUTPUT OF 1ST MARKET ONLY              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     R4,CURRENT                                                       
         LA    R4,FEBL-FEB(R4)                                                  
         ST    R4,CURRENT                                                       
         OC    0(2,R4),0(R4)                                                    
         BNZ   SETHDR                                                           
         CLI   DEMO,3                                                           
         BE    CHKMKT                                                           
         ZIC   R6,DEMO                                                          
         LA    R6,1(R6)                                                         
         STC   R6,DEMO                                                          
         B     SETDEM                                                           
CHKMKT   CLI   MARKET,1                                                         
         BE    ENDJOB                                                           
         ZIC   R6,MARKET                                                        
         LA    R6,1(R6)                                                         
         STC   R6,MARKET                                                        
         MVI   DEMO,0                                                           
         B     SETDEM                                                           
*                                                                               
ENDJOB   CLOSE (OUT)                                                            
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
DEMOTAB  DC    AL1(1,3,9,13)                                                    
DUB      DS    D                   DOUBLE WORD                                  
WORD     DS    F                   FULL WORD                                    
HALF     DS    H                   HALF WORD                                    
CHAR     DS    C                   CHARACTER                                    
CURRENT  DS    F                                                                
MARKET   DC    X'00'                                                            
DEMO     DC    X'00'                                                            
DEM      DS    CL3                                                              
*                                                                               
CARDD    DS    0H                                                               
CMED     DS    CL1                 MEDIA = T                                    
CRAT     DS    CL1                 SERVICE = N                                  
CSRC     DS    CL1                 SOURCE = E/F                                 
         DS    CL1                                                              
CMKT     DS    CL4                 MARKET                                       
         DS    CL1                                                              
CDAY     DS    CL3                 DAY M-F,SAT,SUN                              
         DS    CL1                                                              
CSTQH    DS    CL2                 START QH                                     
         DS    CL1                                                              
CENDQH   DS    CL2                 END QH                                       
         DS    CL1                                                              
CAUD     DS    CL2                 AUDIENCE TYPE = 01                           
         DS    CL1                                                              
CACT     DS    CL1                 ACTION CODE = A                              
         DS    CL1                                                              
CMONS    DS    CL48                MONTHS                                       
         DS    CL8                 SPARE                                        
*                                                                               
         LTORG                                                                  
*                                                                               
DYTMS    DS    0H                            SEPT                               
FEB      DS    0H                                                               
*                DAY       STRT      END      SVI                               
* FEB                                                                           
         DC    C'M-F ',AL2(0900),AL2(1200)                                      
         DC    AL2(262,134,087,092,259,133,088,094)                             
         DC    AL2(236,143,091,095,229,118,075,086)                             
         DC    AL2(263,124,104,087,259,131,114,090)                             
         DC    AL2(251,130,095,093,255,137,091,096)                             
FEBL     DC    C'M-F ',AL2(1200),AL2(1600)                                      
         DC    AL2(293,163,115,110,294,144,109,107)                             
         DC    AL2(258,150,107,103,266,132,086,091)                             
         DC    AL2(276,129,105,092,289,149,116,101)                             
         DC    AL2(295,168,130,117,296,146,109,102)                             
         DC    C'M-F ',AL2(1600),AL2(1800)                                      
         DC    AL2(467,226,207,189,471,212,212,186)                             
         DC    AL2(410,187,189,158,419,185,176,159)                             
         DC    AL2(377,165,145,139,398,196,176,158)                             
         DC    AL2(468,232,225,204,474,206,175,174)                             
         DC    C'M-F ',AL2(1800),AL2(1930)                                      
         DC    AL2(594,352,289,328,608,350,331,347)                             
         DC    AL2(512,263,249,300,533,282,263,282)                             
         DC    AL2(479,254,201,234,497,281,244,266)                             
         DC    AL2(577,352,316,340,598,329,315,322)                             
         DC    C'M-F ',AL2(1930),AL2(2000)                                      
         DC    AL2(692,389,322,367,634,406,377,401)                             
         DC    AL2(549,326,303,317,568,335,296,324)                             
         DC    AL2(511,286,232,272,506,299,261,282)                             
         DC    AL2(608,379,346,375,630,383,383,386)                             
         DC    C'M-F ',AL2(2000),AL2(2300)                                      
         DC    AL2(627,478,385,426,633,477,395,445)                             
         DC    AL2(586,436,390,408,603,446,374,423)                             
         DC    AL2(551,377,334,355,542,387,341,361)                             
         DC    AL2(598,446,396,423,627,459,395,435)                             
         DC    C'M-F ',AL2(2300),AL2(2330)                                      
         DC    AL2(383,245,185,232,396,239,196,234)                             
         DC    AL2(375,231,179,219,371,232,172,223)                             
         DC    AL2(402,236,201,233,386,248,219,219)                             
         DC    AL2(365,219,161,202,379,226,179,220)                             
         DC    C'M-F ',AL2(2330),AL2(2500)                                      
         DC    AL2(205,100,083,100,222,102,095,104)                             
         DC    AL2(206,093,073,091,209,091,072,088)                             
         DC    AL2(229,112,102,107,224,109,103,102)                             
         DC    AL2(194,082,066,077,222,124,101,116)                             
         DC    C'SAT ',AL2(0700),AL2(1300)                                      
         DC    AL2(302,100,111,101,304,101,133,105)                             
         DC    AL2(268,097,115,096,280,092,119,103)                             
         DC    AL2(247,083,094,088,236,085,098,082)                             
         DC    AL2(314,114,136,120,291,090,119,098)                             
         DC    C'SAT ',AL2(1300),AL2(1700)                                      
         DC    AL2(353,197,138,141,372,166,166,165)                             
         DC    AL2(311,112,143,130,397,193,198,210)                             
         DC    AL2(285,108,112,105,287,103,107,112)                             
         DC    AL2(437,208,221,223,385,151,162,168)                             
         DC    C'SAT ',AL2(1700),AL2(1800)                                      
         DC    AL2(478,221,177,203,498,246,207,248)                             
         DC    AL2(393,164,177,172,412,162,152,173)                             
         DC    AL2(364,177,158,161,375,139,133,150)                             
         DC    AL2(536,298,283,296,522,248,214,248)                             
         DC    C'SAT ',AL2(1800),AL2(1900)                                      
         DC    AL2(543,304,263,305,540,313,290,329)                             
         DC    AL2(443,242,239,254,460,249,233,258)                             
         DC    AL2(421,198,194,200,402,188,173,186)                             
         DC    AL2(578,328,324,343,551,305,284,315)                             
         DC    C'SAT ',AL2(1930),AL2(2000)                                      
         DC    AL2(578,403,365,398,585,585,350,360)                             
         DC    AL2(493,310,305,318,510,318,273,313)                             
         DC    AL2(445,263,243,255,438,223,210,223)                             
         DC    AL2(595,395,390,400,593,353,336,360)                             
         DC    C'SAT ',AL2(2000),AL2(2300)                                      
         DC    AL2(565,451,385,397,580,580,379,387)                             
         DC    AL2(519,371,336,353,531,343,294,340)                             
         DC    AL2(489,324,269,309,467,302,243,281)                             
         DC    AL2(565,400,355,387,565,413,357,395)                             
         DC    C'SAT ',AL2(2300),AL2(2330)                                      
         DC    AL2(408,261,240,258,433,433,233,265)                             
         DC    AL2(390,240,188,235,393,220,188,220)                             
         DC    AL2(400,240,193,248,400,240,205,220)                             
         DC    AL2(395,220,193,240,423,255,220,260)                             
         DC    C'SAT ',AL2(2330),AL2(2500)                                      
         DC    AL2(276,169,179,171,295,295,184,185)                             
         DC    AL2(258,130,150,145,280,163,178,166)                             
         DC    AL2(260,155,148,164,268,135,143,131)                             
         DC    AL2(246,135,150,150,287,203,210,208)                             
         DC    C'SUN ',AL2(0700),AL2(1300)                                      
         DC    AL2(308,118,116,124,304,129,138,141)                             
         DC    AL2(271,103,106,112,266,092,094,110)                             
         DC    AL2(257,094,092,102,223,084,083,090)                             
         DC    AL2(315,126,136,146,299,111,127,137)                             
         DC    C'SUN ',AL2(1300),AL2(1700)                                      
         DC    AL2(400,202,195,189,400,181,169,195)                             
         DC    AL2(354,147,175,169,380,186,182,206)                             
         DC    AL2(328,130,160,149,320,139,152,155)                             
         DC    AL2(480,262,285,293,430,187,217,230)                             
         DC    C'SUN ',AL2(1700),AL2(1800)                                      
         DC    AL2(528,304,247,285,525,261,235,273)                             
         DC    AL2(426,219,230,227,468,236,236,258)                             
         DC    AL2(403,177,161,184,406,195,176,195)                             
         DC    AL2(554,358,344,353,560,331,302,328)                             
         DC    C'SUN ',AL2(1800),AL2(1900)                                      
         DC    AL2(587,343,311,347,585,367,350,382)                             
         DC    AL2(486,289,265,297,510,302,282,324)                             
         DC    AL2(448,242,203,232,424,217,202,231)                             
         DC    AL2(613,403,358,397,604,404,375,398)                             
         DC    C'SUN ',AL2(1900),AL2(2300)                                      
         DC    AL2(671,544,439,492,676,547,467,522)                             
         DC    AL2(601,447,421,433,602,473,411,455)                             
         DC    AL2(544,383,330,362,544,379,344,368)                             
         DC    AL2(653,520,462,493,665,536,472,507)                             
         DC    C'SUN ',AL2(2300),AL2(2330)                                      
         DC    AL2(405,240,155,218,400,245,200,240)                             
         DC    AL2(365,220,183,215,353,218,183,203)                             
         DC    AL2(395,215,185,215,368,198,168,190)                             
         DC    AL2(383,205,143,190,395,233,198,215)                             
         DC    C'SUN ',AL2(2330),AL2(2500)                                      
         DC    AL2(191,072,058,069,199,093,108,108)                             
         DC    AL2(173,063,064,078,178,071,055,063)                             
         DC    AL2(215,090,071,088,196,081,073,078)                             
         DC    AL2(181,083,074,083,189,090,072,080)                             
DYTMSX   DC    F'0'                                                             
*                                                                               
*NSIMKTS DC    C'NT',AL3(NSIMKTSX+2)                                            
NSIMKTS  DS    0CL32                                                            
         DC    AL2(0419),CL30'SEATTLE-TACOMA'                                   
         DC    AL2(0420),CL30'PORTLAND,OR'                                      
NSIMKTSX DC    AL2(0)                                                           
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=08000,                                          X        
               MACRF=PM                                                         
*                                                                               
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054DESVIPEP  08/23/00'                                      
         END                                                                    

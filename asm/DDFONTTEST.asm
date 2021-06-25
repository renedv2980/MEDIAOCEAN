*          DATA SET DDFONTTEST AT LEVEL 008 AS OF 05/01/02                      
*PHASE FONTTEST,*                                                               
*INCLUDE PRINT                                                                  
*INCLUDE REGSAVE                                                                
         TITLE 'TESTING OUT MULTIPLE FONTS'                                     
FONTTEST CSECT                                                                  
         NBASE 0,**FONT**,=V(REGSAVE)                                           
         L     RA,=V(BOXAREA)                                                   
         USING BOXD,RA                                                          
         MVI   BOXOPTCD,C'J'       TURN ON MULTIPLE FONTS                       
         BAS   RE,SKIP                                                          
         MVI   FONT,X'00'                                                       
         BAS   RE,BASIC                                                         
         MVI   FONT,X'01'                                                       
         BAS   RE,BASIC                                                         
         BAS   RE,BOTH                                                          
         MVI   FONT,X'00'                                                       
         BAS   RE,BOXSAMPL                                                      
         MVI   FONT,X'01'                                                       
         BAS   RE,BOXSAMPL                                                      
         XBASE                                                                  
         EJECT                                                                  
*              BASIC CHARACTER SET                                              
         SPACE 3                                                                
BASIC    NTR1                                                                   
         MVC   P(19),=C'BASIC CHARACTER SET'                                    
         BAS   RE,P1                                                            
         MVC   P(19),UNDER                                                      
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
         MVC   P+4(30),=C'00  01  02  03  04  05  06  07'                       
         MVC   P+36(30),=C'08  09  0A  0B  0C  0D  0E  0F'                      
         BAS   RE,P2                                                            
         LA    R2,LINELIST                                                      
         LA    R3,16                                                            
         SR    R4,R4                                                            
         SPACE 1                                                                
BASIC2   LA    R5,P+5                                                           
         LA    R6,16                                                            
         MVC   P(2),0(R2)                                                       
         SPACE 1                                                                
BASIC4   STC   R4,0(R5)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,BASIC4                                                        
         BAS   RE,P2                                                            
         SPACE 1                                                                
         LA    R2,2(R2)                                                         
         BCT   R3,BASIC2                                                        
         BAS   RE,SKIP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              BOX SAMPLES                                                      
         SPACE 3                                                                
BOXSAMPL NTR1                                                                   
         MVC   P(19),=C'SAMPLE BOX WEIGHT 1'                                    
         MVC   0(200,RA),MYBOX                                                  
         MVC   200(200,RA),MYBOX+200                                            
         BAS   RE,BOX                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A PAGE WITH A BOX                               
         SPACE 3                                                                
BOX      NTR1                                                                   
         MVI   BOXCOLS+36,C'-'                                                  
         BAS   RE,P1                                                            
         MVC   P(19),UNDER                                                      
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
         MVC   P+10(30),=C'MONTH  HOURS   COST    REMARKS'                      
         MVI   BOXCOLS+36,C'D'                                                  
         BAS   RE,P2                                                            
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'JAN82'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'FEB82'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'MAR82'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'APR82'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'MAY82'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'JUN82'                                                
         BAS   RE,P1                                                            
         BAS   RE,SKIP                                                          
         B     XIT                                                              
         SPACE 1                                                                
MYBOX    DC    C'Y'                                                             
         DC    AL1(1)                                                           
         DC    6X'00'                                                           
         DC    C'         L     C       C      C           R '                  
         DC    C'                                            '                  
         DC    132C' '                                                          
         DC    CL44' '                                                          
         DC    C'    T D        B              '                                
         DC    C'                              '                                
         DC    40C' '                                                           
         DC    28X'00'                                                          
         SPACE 1                                                                
DDS      DC    4XL16'00'                                                        
         DC    AL1(5,29,49,74,97,122,0,0,0,0,0,0,0,0,0,0)                       
         DC    AL1(5,30,49,75,97,122,0,0,0,0,0,0,0,0,0,0)                       
         DC    AL1(5,7,29,31,49,51,74,76,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,97,99,120,122,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,96,98,119,121,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,95,97,118,120,0,0,0,0)                 
         DC    AL1(5,7,30,32,49,51,75,77,94,96,117,119,0,0,0,0)                 
         DC    AL1(5,32,49,77,94,119,0,0,0,0,0,0,0,0,0,0)                       
         DC    AL1(5,32,49,77,94,119,0,0,0,0,0,0,0,0,0,0)                       
         DC    12XL16'00'                                                       
         EJECT                                                                  
*              ROUTINE TO PRINT BOTH FONTS ON SAME LINE                         
         SPACE 3                                                                
BOTH     NTR1                                                                   
         LA    R2,20                                                            
         BAS   RE,BOTH2                                                         
         BCT   R2,*-4                                                           
         BAS   RE,SKIP                                                          
         B     XIT                                                              
         SPACE 1                                                                
BOTH2    NTR1                                                                   
         MVC   P,PAT1                                                           
         MVI   FONT,0                                                           
         BAS   RE,P0                                                            
         MVC   P,PAT1+10                                                        
         MVI   FONT,1                                                           
         BAS   RE,P1                                                            
         MVC   P,PAT1                                                           
         MVI   FONT,1                                                           
         BAS   RE,P0                                                            
         MVC   P,PAT1+10                                                        
         MVI   FONT,0                                                           
         BAS   RE,P1                                                            
         B     XIT                                                              
         SPACE 1                                                                
PAT1     DC    CL40'AAAAAAAAAA          BBBBBBBBBB          '                   
         DC    CL40'CCCCCCCCCC          DDDDDDDDDD          '                   
         DC    CL40'EEEEEEEEEE          FFFFFFFFFF          '                   
         DC    CL40'GGGGGGGGGG          HHHHHHHHHH          '                   
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
PFILL    DS    CL1                                                              
P        DC    CL132' '                                                         
C        DC    CL80' '                                                          
SPACES   DC    CL132' '                                                         
LINELIST DC    C'00102030405060708090A0B0C0D0E0F0'                              
COMMAND  DS    CL4                                                              
PARA     DS    6F                                                               
UNDER    DC    40X'1C'                                                          
GRPHLIST DS    0H                                                               
         DC    C'30',X'30'                                                      
         DC    C'31',X'31'                                                      
         DC    C'32',X'32'                                                      
         DC    C'33',X'33'                                                      
         DC    C'34',X'34'                                                      
         DC    C'35',X'35'                                                      
         DC    C'36',X'36'                                                      
         DC    C'37',X'37'                                                      
         DC    C'38',X'38'                                                      
         DC    C'39',X'39'                                                      
         DC    C'3A',X'3A'                                                      
         DC    C'3B',X'3B'                                                      
         DC    C'3C',X'3C'                                                      
         DC    C'3D',X'3D'                                                      
         DC    C'3E',X'3E'                                                      
         DC    C'3F',X'3F'                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
SKIP     NTR1                                                                   
         MVC   COMMAND,=C'BC01'                                                 
         B     PALL                                                             
         SPACE 1                                                                
P0       NTR1                                                                   
         MVC   COMMAND,=C'BL00'                                                 
         B     PALL                                                             
         SPACE 1                                                                
P1       NTR1                                                                   
         MVC   COMMAND,=C'BL01'                                                 
         B     PALL                                                             
         SPACE 1                                                                
P2       NTR1                                                                   
         MVC   COMMAND,=C'BL02'                                                 
         SPACE 1                                                                
PALL     MVC   BOXFONT,FONT                                                     
         GOTO1 =V(PRINT),PARA,P-1,COMMAND                                       
         MVC   P,SPACES                                                         
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
FONT     DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DDFONTTEST05/01/02'                                      
         END                                                                    

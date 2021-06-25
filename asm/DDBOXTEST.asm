*          DATA SET DDBOXTEST  AT LEVEL 001 AS OF 10/03/13                      
*PHASE BOXTESTA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE BLOWUP                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'SAMPLE CODE TO USE BOXES'                                       
BOXTEST  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**BOX**,=V(REGSAVE),R9                                         
         L     RA,=V(BOXAREA)                                                   
         USING BOXD,RA                                                          
         BAS   RE,SKIP                                                          
***********************************************************************         
*BASIC CHARACTER SET PRINTED IN CODE PAGE FORMAT                      *         
***********************************************************************         
CODEP    MVC   P(15),=C'BASIC CODE PAGE'                                        
         BAS   RE,P1                                                            
         MVC   P(15),UNDER                                                      
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
         MVC   P+04(32),=C'0-  1-  2-  3-  4-  5-  6-  7-  '                    
         MVC   P+36(32),=C'8-  9-  A-  B-  C-  D-  E-  F-  '                    
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
         L     R2,=A(CODELIST)                                                  
         LA    R3,16                                                            
*                                                                               
CODEP2   LA    R5,P+5                                                           
         LA    R6,16                                                            
         LA    R4,2(R2)            R4=A(FIRST CHARACTER IN TABLE)               
         MVC   P(2),0(R2)                                                       
*                                                                               
CODEP4   MVC   0(1,R5),0(R4)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,CODEP4                                                        
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
*                                                                               
         LA    R2,L'CODELIST(R2)                                                
         BCT   R3,CODEP2                                                        
         BAS   RE,SKIP                                                          
         EJECT                                                                  
***********************************************************************         
*BASIC CHARACTER SET                                                  *         
***********************************************************************         
BASIC    MVC   P(19),=C'BASIC CHARACTER SET'                                    
         BAS   RE,P1                                                            
         MVC   P(19),UNDER                                                      
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
         MVC   P+04(30),=C'00  01  02  03  04  05  06  07'                      
         MVC   P+36(30),=C'08  09  0A  0B  0C  0D  0E  0F'                      
         BAS   RE,P2                                                            
         LA    R2,LINELIST                                                      
         LA    R3,16                                                            
         SR    R4,R4                                                            
*                                                                               
BASIC2   LA    R5,P+5                                                           
         LA    R6,16                                                            
         MVC   P(2),0(R2)                                                       
*                                                                               
BASIC4   STC   R4,0(R5)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,BASIC4                                                        
         BAS   RE,P2                                                            
*                                                                               
         LA    R2,2(R2)                                                         
         BCT   R3,BASIC2                                                        
         BAS   RE,SKIP                                                          
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
*GRAPHIC CHARACTERS PAGES                                             *         
***********************************************************************         
         LA    R2,GRPHLIST                                                      
GC2      MVC   P(28),=C'EXAMPLE OF GRAPHIC CHARACTER'                           
         MVC   P+29(2),0(R2)                                                    
         BAS   RE,P1                                                            
         MVC   P(31),UNDER                                                      
         BAS   RE,P2                                                            
         LA    R3,10                                                            
*                                                                               
GC4      MVC   P(1),2(R2)                                                       
         MVC   P+1(131),P                                                       
         BAS   RE,P1                                                            
         BCT   R3,GC4                                                           
         BAS   RE,SKIP                                                          
         LA    R2,3(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   GC2                                                              
         EJECT                                                                  
***********************************************************************         
*BOX SAMPLES                                                          *         
***********************************************************************         
         MVC   P(19),=C'SAMPLE BOX WEIGHT 1'                                    
         MVC   0(200,RA),MYBOX                                                  
         MVC   200(200,RA),MYBOX+200                                            
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX SHADING OUTSIDE'                                    
         MVI   BOXWT,1                                                          
         MVI   BOXSHCH1,BOXSHCQ                                                 
         MVI   BOXSHADE,2                                                       
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX SHADING INSIDE '                                    
         MVI   BOXSHADE,1                                                       
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX PATTERN OVERLAY'                                    
         MVI   BOXSHADE,0                                                       
         MVI   BOXFLASH,C'Y'                                                    
         MVC   C,=CL80'BIG=03 DDS'                                              
         GOTO1 =V(BLOWUP),PARA,C                                                
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX PATTERN OVERLAY'                                    
         MVC   C,=CL80'MID=20 SMALL TOO'                                        
         GOTO1 =V(BLOWUP),PARA,C                                                
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX PATTERN OVERLAY'                                    
         MVC   C,=CL80'MID=30-AND REVERSE'                                      
         GOTO1 =V(BLOWUP),PARA,C                                                
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX PATTERN OVERLAY'                                    
         MVC   C,=CL80'BIG=42-HAMILTON'                                         
         GOTO1 =V(BLOWUP),PARA,C                                                
         BAS   RE,BOX                                                           
*                                                                               
         MVI   BOXSHADE,4                                                       
         MVC   P(19),=C'BOX STRIPES OVERLAY'                                    
         MVI   BOXSHCH1,BOXSHCQ                                                 
         BAS   RE,BOX                                                           
*                                                                               
         MVC   P(19),=C'BOX STRIPES OVERLAY'                                    
         MVI   BOXFLASH,0                                                       
         BAS   RE,BOX                                                           
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*ROUTINE TO PRINT A PAGE WITH A BOX                                   *         
***********************************************************************         
BOX      NTR1                                                                   
         MVI   BOXCOLS+36,C'-'                                                  
         BAS   RE,P1                                                            
         MVC   P(19),UNDER                                                      
         BAS   RE,P2                                                            
         BAS   RE,P2                                                            
         MVC   P+10(30),=C'MONTH  HOURS   COST    REMARKS'                      
         MVI   BOXCOLS+36,C'D'                                                  
         BAS   RE,P2                                                            
         MVC   P+10(5),=C'JAN13'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'FEB13'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'MAR13'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'APR13'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'MAY13'                                                
         BAS   RE,P1                                                            
         MVC   P+10(5),=C'JUN13'                                                
         BAS   RE,P2                                                            
         MVC   P+10(5),=C'TOTAL'                                                
         BAS   RE,P1                                                            
         BAS   RE,SKIP                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*MISCELLANEOUS ROUTINES                                               *         
***********************************************************************         
SKIP     NTR1                                                                   
         MVC   COMMAND,=C'BC01'                                                 
         B     PALL                                                             
*                                                                               
P1       NTR1                                                                   
         MVC   COMMAND,=C'BL01'                                                 
         B     PALL                                                             
*                                                                               
P2       NTR1                                                                   
         MVC   COMMAND,=C'BL02'                                                 
*                                                                               
PALL     GOTO1 =V(PRINT),PARA,P-1,COMMAND                                       
         MVC   P,SPACES                                                         
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*TABLES, CONSTANTS, AND LITERALS                                      *         
***********************************************************************         
BOXSHCQ  EQU   BOX_SH              BOX SHADING CHARACTER (X'42')                
*                                                                               
MYBOX    DC    C'Y'                                                             
         DC    AL1(1)                                                           
         DC    6X'00'                                                           
*                                                                               
MYBCOLS  DC    CL132'         L     C       C      C           R'               
         DC    CL132' '                                                         
*                                                                               
MYBROWS  DC    CL100'    T D      M B              '                            
*                                                                               
         DC    28X'00'                                                          
*                                                                               
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
*                                                                               
PFILL    DS    CL1                                                              
P        DC    CL132' '                                                         
C        DC    CL80' '                                                          
SPACES   DC    CL132' '                                                         
LINELIST DC    C'00102030405060708090A0B0C0D0E0F0'                              
COMMAND  DS    CL4                                                              
PARA     DS    6F                                                               
UNDER    DC    40X'BF'                                                          
*                                                                               
GRPHLIST DS    0CL3                                                             
         DC    C'42',X'42'                                                      
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*HEX CHRS 20 THRU 2D ARE DATA DICTIONARY ESCAPE CHARACTERS            *         
*SET BOXDDCTL=X'80' TO ACTIVATE DATA DICT AND SET 20-2D TO SPACES     *         
***********************************************************************         
CODELIST DS    0CL18                                                            
         DC    C'-0',X'00102030405060708090A0B0C0D0E0F0'                        
         DC    C'-1',X'01112131415161718191A1B1C1D1E1F1'                        
         DC    C'-2',X'02122232425262728292A2B2C2D2E2F2'                        
         DC    C'-3',X'03132333435363738393A3B3C3D3E3F3'                        
         DC    C'-4',X'04142434445464748494A4B4C4D4E4F4'                        
         DC    C'-5',X'05152535455565758595A5B5C5D5E5F5'                        
         DC    C'-6',X'06162636465666768696A6B6C6D6E6F6'                        
         DC    C'-7',X'07172737475767778797A7B7C7D7E7F7'                        
         DC    C'-8',X'08182838485868788898A8B8C8D8E8F8'                        
         DC    C'-9',X'09192939495969798999A9B9C9D9E9F9'                        
         DC    C'-A',X'0A1A2A3A4A5A6A7A8A9AAABACADAEAFA'                        
         DC    C'-B',X'0B1B2B3B4B5B6B7B8B9BABBBCBDBEBFB'                        
         DC    C'-C',X'0C1C2C3C4C5C6C7C8C9CACBCCCDCECFC'                        
         DC    C'-D',X'0D1D2D3D4D5D6D7D8D9DADBDCDDDEDFD'                        
         DC    C'-E',X'0E1E2E3E4E5E6E7E8E9EAEBECEDEEEFE'                        
         DC    C'-F',X'0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFFF'                        
         EJECT                                                                  
*DDBIGBOX                                                                       
       ++INCLUDE DDBIGBOX                                                       
                                                                                
*DDBOXEQUS                                                                      
       ++INCLUDE DDBOXEQUS                                                      
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDBOXTEST 10/03/13'                                      
         END                                                                    

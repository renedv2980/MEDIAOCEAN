*          DATA SET GAMAM02    AT LEVEL 004 AS OF 05/01/02                      
*PHASE TB0D02A                                                                  
         TITLE 'GAMAM02 - MAMMELS'                                              
         PRINT NOGEN                                                            
MAMMALSB CSECT                                                                  
         NMOD1 0,**MAMB**                                                       
*                                                                               
         USING MAMSCRN,RA                                                       
*                                                                               
         CLI   STAGE,X'B0'                                                      
         BE    STGB0                                                            
         CLI   STAGE,X'B1'                                                      
         BE    STGB1                                                            
         CLI   STAGE,X'B2'                                                      
         BE    STGB2                                                            
*                                                                               
STGB0    XC    MAMTOP1,MAMTOP1                                                  
         TWAXC MAMTOP2H,MAMBOT4H,PROT=Y                                         
         MVC   MAMTOP1(L'MESSVII),MESSVII                                       
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
         MVC   QNLN1(L'MESSVI),MESSVI                                           
         MVC   MAMQN01,QNLN1                                                    
         MVI   STAGE,X'B1'                                                      
         B     MAMBXIT                                                          
*                                                                               
STGB1    LA    R1,MAMAN01                                                       
         LA    R2,MAMAN01+L'MAMAN01-1                                           
         XC    MAMTOP1,MAMTOP1                                                  
         XC    MAMTOP2,MAMTOP2                                                  
         XC    MAMQN01,MAMQN01                                                  
*                                                                               
GB110    CLI   0(R1),C'A'          LOOK FOR START AND END OF NAME               
         BNL   GB120                                                            
         CR    R1,R2                                                            
         BE    STGB0               HE HASN'T GIVEN ME ANY INFO                  
         LA    R1,1(R1)                                                         
         B     GB110                                                            
*                                                                               
GB120    CLI   0(R2),C'A'                                                       
         BNL   GB130                                                            
         BCT   R2,GB120                                                         
*                                                                               
GB130    SR    R2,R1                                                            
         LA    R2,1(R2)                                                         
         STC   R2,LHNAME                                                        
         ST    R1,AHNAME                                                        
*                                                                               
GB140    L     RF,ACMPARE          LOOK FOR HIS NAME IN MY LISTS                
         BASR  RE,RF                                                            
*                                                                               
         CLI   #1,X'FF'            IS HIS NAME IN MY LISTS                      
         BNE   GB170               FOUND IT                                     
*                                                                               
         MVI   STAGE,X'00'                                                      
         MVC   MAMTOP1(L'MESSII),MESSII                                         
*                                                                               
         LA    R1,MAMTOP1+L'MESSII                                              
*                                                                               
         L     RF,AINSERT          INSERT HIS NAME IN MESSAGE                   
         BASR  RE,RF                                                            
*                                                                               
         MVC   MAMTOP2(L'MESSV),MESSV                                           
         XC    MAMAN01,MAMAN01                                                  
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
         MVC   QNLN1(L'MESSVIII),MESSVIII                                       
         MVC   MAMQN01,QNLN1                                                    
*                                                                               
         B     MAMBXIT                                                          
*                                                                               
*    HIS NAME IS IN MY LISTS,SHOW HIM QNS & ANSWERS.                            
*                                                                               
GB170    MVI   STAGE,X'B2'                                                      
         MVC   MAMTOP1(L'MESSIII),MESSIII                                       
         LA    R1,MAMTOP1+L'MESSIII                                             
*                                                                               
         L     RF,AINSERT                                                       
         BASR  RE,RF                                                            
*                                                                               
         L     R3,AHNAME                                                        
         ZIC   R2,LHNAME                                                        
         BCTR  R2,0                                                             
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CANDSAS(0),0(R3)    SAVE HIS NAME IN THE TWA AT CANDSAS          
*                                                                               
         L     R1,PARM                                                          
         MVC   0(L'MESSIV,R1),MESSIV                                            
         XC    MAMAN01,MAMAN01                                                  
*                                                                               
STGB2    L     R1,ATABLES                                                       
         OC    #3,#3                                                            
         BNZ   B10                                                              
         OC    #2,#2                                                            
         BNZ   B20                                                              
         OC    #1,#1                                                            
         BNZ   B30                                                              
*                                                                               
B10      ZIC   R3,#3                                                            
         XC    #3,#3                                                            
         L     R2,ADTABH3                                                       
         B     B40                                                              
*                                                                               
B20      ZIC   R3,#2                                                            
         XC    #2,#2                                                            
         L     R2,ADTABH2                                                       
         B     B40                                                              
*                                                                               
B30      ZIC   R3,#1                                                            
         XC    #1,#1                                                            
         MVI   STAGE,X'00'         THIS IS LAST STAGE                           
         L     R2,ADTABH1                                                       
*                                                                               
B40      LA    R2,0(R2,R1)                                                      
         MVC   STTABH,0(R2)                                                     
         L     RF,ARELOC                                                        
         BASR  RE,RF                                                            
*                                                                               
         BCTR  R3,0                                                             
         L     R4,LEN                                                           
         SR    R2,R2                                                            
         MR    R2,R4                                                            
         L     R2,ACHTAB                                                        
         LA    R2,0(R2,R3)                                                      
*                                                                               
         BCTR  R4,0                                                             
         LA    R5,0(R4,R2)                                                      
         LA    R4,1                                                             
*                                                                               
         LR    R3,R2                                                            
         BCTR  R2,0                                                             
*                                                                               
B50      CLI   0(R3),X'80'         IF NOT X'80' MUST BE Y OR N                  
         BNL   B60                                                              
         SR    R3,R2                                                            
         STC   R3,QNNUM                                                         
         AR    R3,R2                                                            
         L     RF,AASKQN           ROLL DOWN & MOVE QN INTO TWA                 
         BASR  RE,RF                                                            
*                                                                               
         OC    IND,IND             DID THIS QN TAKE TWO LINES                   
         BZ    *+12                                                             
         LA    R6,MAMAN02                                                       
         B     *+8                                                              
         LA    R6,MAMAN01                                                       
*                                                                               
         CLI   0(R3),Y                                                          
         BNE   B55                                                              
         MVC   0(3,R6),=C'YES'                                                  
         B     B60                                                              
*                                                                               
B55      MVC   0(2,R6),=C'NO'                                                   
B60      BXLE  R3,R4,B50                                                        
         CLI   STAGE,X'00'                                                      
         BNE   MAMBXIT                                                          
         XC    MAMTOP1,MAMTOP1                                                  
         MVC   MAMTOP1(L'MESSI),MESSI                                           
         LA    R1,MAMTOP1+L'MESSI                                               
*                                                                               
         LA    R2,CANDSAS                                                       
         ST    R2,AHNAME                                                        
         L     RF,AINSERT                                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   MAMTOP2(L'MESSV),MESSV                                           
*                                                                               
         MVI   QNLN1,C'.'                                                       
         MVC   QNLN1+1(L'QNLN1-1),QNLN1                                         
         MVC   QNLN1(L'MESSVIII),MESSVIII                                       
*                                                                               
         MVI   NOLNS,X'01'                                                      
         XC    IND,IND                                                          
         L     RF,AFIXSCRN                                                      
         BASR  RE,RF                                                            
MAMBXIT  XMOD1 1                                                                
         EJECT                                                                  
*   EQUATES STATEMENTS                                                          
Y        EQU   X'02'                                                            
N        EQU   X'01'                                                            
D        EQU   X'80'                                                            
WRONG    EQU   X'03'                                                            
LAST     EQU   X'FF'                                                            
*                                                                               
*                                                                               
*     MESSAGES                                                                  
*                                                                               
MESSI    DC    C'THAT''S ALL I KNOW ABOUT A'                                    
MESSII   DC    C'I''M AFRAID I''VE NEVER HEARD OF A'                            
MESSIII  DC    C'FOR MORE INFORMATION ON A'                                     
MESSIV   DC    C',HIT ENTER'                                                    
MESSV    DC    C'TO PLAY AGAIN ENTER ''A'' OR ''B'''                            
MESSVI   DC    C'PLEASE KEY IN YOUR NAME'                                       
MESSVII  DC    C'HELLO,THIS IS VERSION B '                                      
MESSVIII DC    C'YOU ARE NOW PLAYING VERSION B'                                 
         EJECT                                                                  
*   LITERALS/DSECTS                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE GAMAMTW                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GAMAM02   05/01/02'                                      
         END                                                                    

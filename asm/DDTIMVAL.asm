*          DATA SET DDTIMVAL   AT LEVEL 023 AS OF 04/01/05                      
*PHASE T00A0EA                                                                  
         TITLE 'MODULE TO VALIDATE TIME INPUT'                                  
****************************CAREFUL*******************************              
*                                                                *              
*   *** IF YOU CHANGE THIS, PLEASE CHANGE DDTIMVALR ALSO ***     *              
*                                                                *              
* AT SOME POINT DURING THE HISTORY OF DDTIMVAL, IT WAS CHANGED   *              
* FROM A *CATALP TO A CORE-RESIDENT PHASE. HOWEVER:              *              
*  1. RMTIMVAL REMAINED ON THE LIVE PAN LIBRARY AT LEVEL 15 AS   *              
*     OF 6/13/84.                                                *              
*  2. THE CORRESPONDING SOURCE FOR THAT RELO WAS NOT MAINTAINED. *              
*  3. CHANGES WERE SUBSEQUENTLY MADE TO DDTIMVAL WHICH WERE NOT  *              
*     REFLECTED IN THE RELO VERSION.                             *              
*  4. SOME OF THOSE CHANGES WERE HELPFUL, BUT SOME MAY BE UNSAFE *              
*     FOR THE APPLICATIONS THAT HAVE ALWAYS USED THE RELO        *              
*     VERSION (E.G., SUPPORT FOR "NONE" AND "VAR").              *              
*                                                                *              
* THEREFORE, DEIS HAS RECONSTRUCTED THE CATALP VERSION OF        *              
* DDTIMVAL AND CALLED IT DDTIMVALR. IT WAS CREATED FROM THE      *              
* LEVEL 22 VERSION OF DDTIMVAL, BUT DOES NOT CONTAIN THE "NONE"  *              
* AND "VAR" CODE BELOW, LEST IT CAUSE PROBLEMS FOR EXISTING      *              
* APPLICATIONS. THE OTHER CHANGES WERE LEFT IN (BECAUSE THEY     *              
* WERE DEEMED TO BE SAFE), AND THEY ARE INDICATED IN DDTIMVALR   *              
* WITH COMMENTS.                                                 *              
*                                                                *              
* BOTTOM LINE: DDTIMVAL AND DDTIMVALR SHOULD REMAIN IN SYNC,     *              
* EXCEPT FOR SPECIFIC DIFFERENCES WHICH ARE THOUGHT TO BE UNSAFE *              
* IN DDTIMVALR.                                                  *              
*                                                                *              
*                                    -- DEIS (MAR31/05)          *              
*                                                                *              
* 16JUN00 TESTS FOR 12A AND 12P WERE BACKWARDS. FIXED. MHER      *              
*                                                                *              
* SOMEWHERE ALONG THE LINE THE RELO VERSION OF THIS WAS CHANGED  *              
* TO EXCLUDE THE NONE AND VAR PORTIONS OF THIS CODE. BE SURE     *              
* TO COMMENT OUT THOSE PORTIONS IF A CATALP IS DONE.             *              
******************************************************************              
TIMVAL   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 03,**TMVL**                                                      
         USING TIMD,RC                                                          
         LM    R2,R3,0(R1)         PARAMETER 1 BYTE 0   L'INPUT                 
         SR    R4,R4                                1-3 A(INPUT)                
         IC    R4,0(R1)                      2          A(OUTPUT)               
         SLL   R2,8                                                             
         SRL   R2,8                                                             
         SPACE 2                                                                
*                                  AFTER TIMVAL P1 BYTE 0 X'FF' INVALID         
         EJECT                                                                  
*              ESTABLISH LENGTH OF FIELDS                                       
         SPACE 3                                                                
         CLC   0(4,R2),=C'NONE'    SPECIAL NONE CONDITION                       
         BNE   TM2                                                              
         MVC   0(4,R3),=C'NONE'                                                 
TM1      MVI   0(R1),0                                                          
         B     TMEXT                                                            
         SPACE 2                                                                
TM2      CLC   0(3,R2),=C'VAR'                                                  
         BNE   TM2A                                                             
         MVC   0(4,R3),=C'VARY'                                                 
         B     TM1                                                              
         SPACE 2                                                                
TM2A     DS    0H                                                               
         LR    R5,R2                                                            
         LR    R6,R4                                                            
         SPACE 2                                                                
TM4      CLI   0(R5),C'-'          LOOK FOR A DASH                              
         BE    TM6                                                              
         LA    R5,1(R5)                                                         
         BCT   R6,TM4                                                           
         SR    R5,R5               NOT THERE - SO ONLY ONE TIME                 
         B     TM8                                                              
         SPACE 2                                                                
TM6      SR    R5,R2                                                            
         SR    R4,R5                                                            
         BCTR  R4,R0                                                            
         XR    R4,R5                                                            
         XR    R5,R4                                                            
         XR    R4,R5                                                            
         LTR   R5,R5                                                            
         BNP   TMERR                                                            
         SPACE 2                                                                
TM8      CH    R4,=H'1'                                                         
         BL    TMERR                                                            
         CH    R4,=H'5'                                                         
         BH    TMERR                                                            
         CH    R5,=H'5'                                                         
         BH    TMERR                                                            
         EJECT                                                                  
*              NOW ESTABLISH FIXED LENGTH FIELDS                                
         SPACE 3                                                                
         MVC   TIME1(10),=10C'0'                                                
         LA    R6,TIME1+5          FIRST FIELD                                  
         SR    R6,R4                                                            
         BCTR  R4,0                                                             
         EX    R4,TM10                                                          
*                                                                               
         BAS   RE,NORM             NOON OR MID                                  
         BNE   *+10                                                             
         MVC   TIME1(5),DUB                                                     
*                                                                               
         CLI   LET1,C'Z'                                                        
         BL    *+10                                                             
         MVC   TIME1,TIME1+1                                                    
         CLC   TIME1(2),=C'00'                                                  
         BNE   TM11                                                             
         MVC   TIME1(2),TIME1+2                                                 
         MVC   TIME1+2(2),=C'00'                                                
         B     TM11                                                             
         SPACE 2                                                                
TM10     MVC   0(0,R6),0(R2)                                                    
         SPACE 1                                                                
TM11     LTR   R5,R5                                                            
         BZ    TM12                                                             
         LA    R2,2(R2,R4)         SECOND FIELD                                 
         LA    R6,TIME2+5                                                       
         SR    R6,R5                                                            
         BCTR  R5,0                                                             
         EX    R5,TM10                                                          
*                                                                               
         BAS   RE,NORM             NOON OR MID                                  
         BNE   *+10                                                             
         MVC   TIME2(5),DUB                                                     
*                                                                               
         CLC   TIME2+3(2),=C'CC'                                                
         BE    TM12                                                             
         CLC   TIME2(2),=C'00'                                                  
         BNE   TM12                                                             
         MVC   TIME2(2),TIME2+2                                                 
         MVC   TIME2+2(2),=C'00'                                                
         SPACE 2                                                                
TM12     CLI   LET1,C'Z'           IS THE LETTER OMITTED                        
         BL    TM14                FROM THE FIRST FIELD                         
         MVC   LET1,LET2           ASSUME SECOND VALUE                          
         CLI   LET1,C'N'                                                        
         BNE   *+8                                                              
         MVI   LET1,C'A'           NOON BECOMES AM                              
         CLI   LET1,C'M'                                                        
         BNE   *+8                                                              
         MVI   LET1,C'P'           MIDNIGHT BECOMES PM                          
         SPACE 1                                                                
         CLI   LET2,C'A'           IF SECOND AM                                 
         BNE   TM14                                                             
         CLC   TIME1,TIME2                                                      
         BNH   TM14                AND FIRST IS HIGH                            
         MVI   LET1,C'P'           MAKE IT PM                                   
         CLC   TIME1,=C'1200'                                                   
         BL    TM14                                                             
         MVI   LET1,C'M'                                                        
         CLC   TIME1,=C'1200'                                                   
         BE    TM14                                                             
         MVI   LET1,C'A'                                                        
         EJECT                                                                  
*              BIT OF LOGIC ON THE FIELDS                                       
         SPACE 3                                                                
TM14     LA    R5,TIME1            CHECK FIRST TIME                             
         LA    R6,MIL1                                                          
         BAS   RE,VALTIM                                                        
         XC    MIL2,MIL2                                                        
         CLC   TIME2,=10C'0'       IF SECOND IS MISSING WERE DONE               
         BE    TMOK                                                             
         CLC   TIME2+3(2),=C'CC'                                                
         BNE   *+14                                                             
         MVC   MIL2,=C'CC'                                                      
         B     TMOK                                                             
         LA    R5,TIME2            CHECK SECOND TIME                            
         LA    R6,MIL2                                                          
         BAS   RE,VALTIM                                                        
         CLC   MIL2,MIL1           IF SECOND TIME IS LESS THAN FIRST            
         BH    TMOK                                                             
         CLC   MIL2,=H'600'        THEN IF MUST BE BEFORE 6AM                   
         BL    TMOK                                                             
         B     TMERR                                                            
         EJECT                                                                  
NORM     LA    RF,1                CHECK FOR NOON OR MID INPUT                  
         CLC   0(3,R2),=C'NOO'                                                  
         BE    *+10                                                             
         CLC   0(3,R2),=C'MID'                                                  
         BNE   NORM2                                                            
*                                                                               
         SR    RF,RF               FOR NOON OR MID                              
         MVC   DUB(4),=C'1200'     SET TIME IN DUB                              
         MVC   DUB+4(1),0(R2)                                                   
*                                                                               
NORM2    LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
*              CHECK TIMES AND CONVERT TO MILITARY                              
         SPACE 3                                                                
VALTIM   CLC   0(2,R5),=C'00'      9 BECOMES 900, 10 BECOMES 1000               
         BNE   TM20                                                             
         MVC   0(2,R5),2(R5)                                                    
         MVC   2(2,R5),=C'00'                                                   
         SPACE 2                                                                
TM20     MVC   0(2,R6),=H'1200'    NOON AND MIDNIGHT CHECK                      
         CLC   0(5,R5),=C'1200N'                                                
         BER   RE                                                               
         CLC   0(5,R5),=C'1200P'                                                
         BER   RE                                                               
         MVC   0(2,R6),=H'2400'                                                 
         CLC   0(5,R5),=C'1200M'                                                
         BER   RE                                                               
         CLC   0(5,R5),=C'1200A'                                                
         BER   RE                                                               
         SPACE 2                                                                
         MVC   DUB(4),=10C'0'      NUMERIC CHECK                                
         MVZ   DUB(4),0(R5)                                                     
         CLC   DUB(4),=10C'0'                                                   
         BNE   TMERR                                                            
         SPACE 2                                                                
         CLC   0(2,R5),=C'01'      HOURS TESTS                                  
         BL    TMERR                                                            
         CLC   0(2,R5),=C'12'                                                   
         BH    TMERR                                                            
         SPACE 2                                                                
         CLC   2(2,R5),=C'59'      MINUTES TEST                                 
         BH    TMERR                                                            
         SPACE 2                                                                
         PACK  DUB,0(4,R5)         CONVERT TO CALENDAR                          
         CVB   R0,DUB                                                           
         STH   R0,0(R6)                                                         
         CLC   0(2,R5),=C'12'                                                   
         BE    TM22                                                             
         CLI   4(R5),C'A'          MORNING NOW OK                               
         BCR   8,RE                                                             
         AH    R0,=H'1200'                                                      
         STH   R0,0(R6)                                                         
         CLI   4(R5),C'P'          ADD 1200 FOR PM TIMES                        
         BCR   8,RE                                                             
         B     TMERR                                                            
         SPACE 2                                                                
TM22     CLI   4(R5),C'P'          BUT FOR TIMES THAT START WITH                
         BCR   8,RE                12, THINGS ARE A LITTLE DIFFERENT            
         SH    R0,=H'1200'                                                      
         STH   R0,0(R6)                                                         
         CLI   4(R5),C'A'                                                       
         BCR   8,RE                                                             
         SPACE 2                                                                
TMERR    XC    0(4,R3),0(R3)                                                    
         MVI   0(R1),X'FF'                                                      
         B     TMEXT                                                            
         SPACE 2                                                                
TMOK     MVC   0(4,R3),MIL1                                                     
         MVI   0(R1),0                                                          
         SPACE 2                                                                
TMEXT    XMOD1 1                                                                
         EJECT                                                                  
*              DSECT FOR TIMVAL                                                 
         SPACE 3                                                                
TIMD     DSECT                                                                  
DUB      DS    D                                                                
TIME1    DS    CL4                                                              
LET1     DS    CL1                                                              
TIME2    DS    CL4                                                              
LET2     DS    CL1                                                              
MIL1     DS    CL2                                                              
MIL2     DS    CL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023DDTIMVAL  04/01/05'                                      
         END                                                                    

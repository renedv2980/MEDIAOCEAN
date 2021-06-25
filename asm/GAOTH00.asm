*          DATA SET GAOTH00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TB0C00A                                                                  
         TITLE 'THE NOBLE AND ELEGANT GAME OF OTHELLO'                          
         PRINT NOGEN                                                            
TB0C00   CSECT                                                                  
         NMOD1 0,**OTHLLO,RR=R7                                                 
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING TB0C00+4096,R9                                                   
         L     RA,4(R1)                                                         
         USING TB0CFFD,RA                                                       
         SPACE 2                                                                
         LA    R2,OTHMOVEH                                                      
         CLI   FIRSTIME,0          INITIALIZATION                               
         BNE   YOU                                                              
         MVI   FIRSTIME,1                                                       
         MVC   BOARD(64),ORIGINAL  SET UP BOARD                                 
         CLI   5(R2),0             NO INPUT - I MOVE FIRST                      
         BE    ME                                                               
         EJECT                                                                  
*              MAIN LOGIC OF GAME - YOUR MOVE                                   
         SPACE 3                                                                
YOU      CLC   8(2,R2),=C'PASS'                                                 
         BE    ME                                                               
         LA    R3,MESSD                                                         
         LA    R4,BOARDLET                                                      
         LA    R5,1                                                             
         SPACE 2                                                                
YOU2     STC   R5,MOVENO                                                        
         CLC   8(2,R2),0(R4)       DO I RECOGNIZE YOUR MOVE                     
         BE    YOU4                                                             
         LA    R4,2(R4)                                                         
         LA    R5,1(R5)                                                         
         CH    R5,=H'65'                                                        
         BL    YOU2                                                             
         B     ALLOUT              NO - EXIT                                    
         SPACE 2                                                                
YOU4     LA    R3,MESSF            YES - IS POSITION OCCUPIED                   
         LA    R5,BOARD-1(R5)                                                   
         CLI   0(R5),0                                                          
         BNE   ALLOUT              YES - EXIT                                   
         SPACE 2                                                                
         LA    R3,MESSE            NO - DOES YOUR MOVE FLIP                     
         BAS   RE,INVERT                                                        
         BAS   RE,FLIP                                                          
         BAS   RE,INVERT                                                        
         CLI   MOVENO,0                                                         
         BE    ALLOUT              NO - EXIT                                    
         EJECT                                                                  
*              MAIN LOGIC OF GAME - MY MOVE                                     
         SPACE 3                                                                
ME       LA    R2,OTHBH                                                         
         BAS   RE,OUTBOARD         FORMAT RIGHT HAND BOARD                      
         BAS   RE,ISITOVER         CHECK END OF GAME                            
         LA    R2,64               SET UP TO TRY MY MOVES                       
         MVC   MYMOVE,SPACES                                                    
         MVC   BESTB,BOARD                                                      
         MVC   SAVBOARD,BOARD                                                   
         XC    BEST,BEST                                                        
         SPACE 2                                                                
ME2      MVC   BOARD,SAVBOARD                                                   
         STC   R2,MOVENO                                                        
         LA    R4,BOARD-1(R2)      IS THIS SPOT OCCUPIED                        
         CLI   0(R4),0                                                          
         BNE   ME4                                                              
         BAS   RE,FLIP                                                          
         CLI   MOVENO,0            DID I GET ANY FLIPS                          
         BE    ME4                                                              
         BAS   RE,VALUE                                                         
         L     RE,SCORE                                                         
         L     RF,BEST                                                          
         CR    RE,RF               WAS THIS MY BEST TO DATE                     
         BL    ME4                                                              
         MVC   BESTB,BOARD         YES - SO SAVE PRESENT POSITION               
         MVC   BEST,SCORE                                                       
         LR    R4,R2                                                            
         BCTR  R4,0                                                             
         SLL   R4,1                                                             
         LA    R4,BOARDLET(R4)                                                  
         MVC   MYMOVE,0(R4)                                                     
         SPACE 2                                                                
ME4      BCT   R2,ME2              TRIED ALL THE MOVES                          
         MVC   BOARD,BESTB         TAKE MY BEST                                 
         LA    R2,OTHAH            FORMAT LEFT BOARD                            
         BAS   RE,OUTBOARD                                                      
         BAS   RE,ISITOVER         CHECK END OF GAME                            
         LA    R3,MESSA                                                         
         CLC   MYMOVE,SPACES       IF WE BOTH PASS, THE GAME IS OVER            
         BNE   ALLOUT                                                           
         CLC   OTHMOVE(4),=C'PASS'                                              
         BE    ISIT2                                                            
         B     ALLOUT                                                           
         EJECT                                                                  
*              EXITS FROM GAME                                                  
         SPACE 3                                                                
ISITOVER LR    R0,RE                                                            
         BAS   RE,ADDBOARD                                                      
         L     R1,MYMEN                                                         
         A     R1,YOURMEN                                                       
         LR    RE,R0                                                            
         CH    R1,=H'64'                                                        
         BCR   7,RE                                                             
         SPACE 2                                                                
ISIT2    MVI   FIRSTIME,0                                                       
         LA    R3,MESSB                                                         
         CLC   YOURMEN,MYMEN                                                    
         BH    ALLOUT                                                           
         LA    R3,MESSC                                                         
         BL    ALLOUT                                                           
         LA    R3,MESSG                                                         
         SPACE 2                                                                
ALLOUT   BAS   RE,HEADOUT                                                       
         OI    OTHMOVEH+6,X'40'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO APPLY FLIPS                                           
         SPACE 3                                                                
FLIP     NTR1                                                                   
         MVC   SAVEMOVE,MOVENO                                                  
         ZIC   R2,MOVENO           POSITION TO RELEVANT FLIP TABLES             
         MVI   MOVENO,0                                                         
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         LA    R2,BOARDADS(R2)                                                  
         L     R2,0(R2)                                                         
         AR    R2,R7               (RELOCATE)                                   
         SPACE 2                                                                
FLIP2    BAS   RE,ROW              TRY THIS ROW                                 
         SPACE 2                                                                
FLIP4    LA    R2,1(R2)            FIND END OF ROW                              
         CLI   0(R2),0                                                          
         BNE   FLIP4                                                            
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'FF'         IS THIS THE LAST ROW                         
         BNE   FLIP2               NO - GO BACK FOR NEXT                        
         B     XIT                                                              
         SPACE 2                                                                
ROW      NTR1                                                                   
         XC    WORK,WORK           BUILD PATTERN IN WORK                        
         LA    R5,WORK                                                          
         LR    R3,R2                                                            
         SPACE 2                                                                
ROW2     ZIC   R4,0(R3)                                                         
         LA    R4,BOARD-1(R4)                                                   
         MVC   0(1,R5),0(R4)                                                    
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         CLI   0(R3),0                                                          
         BNE   ROW2                                                             
         SPACE 2                                                                
         CLI   WORK,2              A VALID FLIP ROW SHOULD LOOK LIKE            
         BNE   XIT                 THIS -  2 (2) (2) ... 1                      
*                                  SO FIRST MUST BE A 2                         
         LA    R5,WORK                                                          
         SPACE 2                                                                
ROW4     CLI   0(R5),1                                                          
         BL    XIT                 SO ZERO MEANS NO FLIP                        
         BE    ROW6                                                             
         LA    R5,1(R5)                                                         
         B     ROW4                                                             
         SPACE 2                                                                
ROW6     MVI   MOVENO,1            AND 1 MEANS FLIP IS ON                       
         ZIC   R4,SAVEMOVE                                                      
         LA    R4,BOARD-1(R4)                                                   
         MVI   0(R4),1             SO ITS OK TO MOVE                            
         SPACE 2                                                                
ROW8     ZIC   R4,0(R2)            NOW APPLY THE FLIP                           
         LA    R4,BOARD-1(R4)      (R2 STILL ADDRESSES ROW)                     
         CLI   0(R4),2                                                          
         BNE   XIT                                                              
         MVI   0(R4),1             CHANGE 2 TO 1                                
         LA    R2,1(R2)                                                         
         B     ROW8                                                             
         EJECT                                                                  
*              ROUTINE TO VALUE THE BOARD                                       
         SPACE 3                                                                
VALUE    NTR1                                                                   
         LA    R2,BOARD                                                         
         LA    R3,VALUETAB                                                      
         LA    R4,64                                                            
         LA    R5,4000                                                          
         SPACE 2                                                                
VALUE1   ZIC   R6,0(R3)            KEY SQUARE VALUE (0-30)                      
         L     R0,MYMEN                                                         
         A     R0,YOURMEN                                                       
         LA    RF,54                                                            
         CR    R0,RF                                                            
         BL    *+6                                                              
         SR    R6,R6                                                            
         LA    R6,2(R6)            PLUS VALUE OF 1 PIECE (2)                    
         CLI   0(R2),1             WHO OCCUPIES SQUARE                          
         BH    VALUE2                                                           
         BE    VALUE4                                                           
         SR    R6,R6               NOBODY                                       
         B     VALUE4                                                           
         SPACE 2                                                                
VALUE2   LCR   R6,R6               YOU DO - COMPLEMENT VALUE                    
         SPACE 2                                                                
VALUE4   AR    R5,R6                                                            
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,VALUE1                                                        
         ST    R5,SCORE                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INVERT THE BOARD                                      
         SPACE 3                                                                
INVERT   NTR1                                                                   
         LA    R2,BOARD                                                         
         LA    R3,64                                                            
         SPACE 2                                                                
INVERT2  CLI   0(R2),1                                                          
         BL    INVERT6                                                          
         BH    INVERT4                                                          
         MVI   0(R2),2                                                          
         B     INVERT6                                                          
         SPACE 2                                                                
INVERT4  MVI   0(R2),1                                                          
         SPACE 2                                                                
INVERT6  LA    R2,1(R2)                                                         
         BCT   R3,INVERT2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD UP THE BOARD                                      
         SPACE 3                                                                
ADDBOARD NTR1                                                                   
         LA    R2,BOARD                                                         
         LA    R3,64                                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         SPACE 2                                                                
AB2      CLI   0(R2),1                                                          
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         CLI   0(R2),2                                                          
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,AB2                                                           
         ST    R4,MYMEN                                                         
         ST    R5,YOURMEN                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT HEADLINE                                       
         SPACE 3                                                                
HEADOUT  NTR1                                                                   
         MVC   OTHHEAD(50),0(R3)                                                
         MVC   OTHHEAD+50(10),SPACES                                            
         OI    OTHHEADH+6,X'80'                                                 
         LA    R2,OTHHEAD                                                       
         LA    R3,50                                                            
         SPACE 2                                                                
HO2      CLC   0(2,R2),=C'XX'                                                   
         BNE   HO4                                                              
         MVC   0(2,R2),MYMOVE                                                   
         CLC   MYMOVE,SPACES                                                    
         BNE   HO4                                                              
         MVC   OTHHEAD(13),=C'I HAD TO PASS'                                    
         SPACE 2                                                                
HO4      CLC   0(2,R2),=C'MM'                                                   
         BNE   HO6                                                              
         EDIT  (4,MYMEN),(2,0(R2))                                              
         SPACE 2                                                                
HO6      CLC   0(2,R2),=C'NN'                                                   
         BNE   HO8                                                              
         EDIT  (4,YOURMEN),(2,0(R2))                                            
         SPACE 2                                                                
HO8      LA    R2,1(R2)                                                         
         BCT   R3,HO2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT A BOARD                                        
         SPACE 3                                                                
OUTBOARD NTR1                                                                   
         LA    R3,BOARD                                                         
         LA    R4,8                8 ROWS                                       
         SPACE 2                                                                
OB2      LA    R5,WORK                                                          
         LA    R6,8                OF 8                                         
         MVC   WORK(3),=C'.  '                                                  
         MVC   WORK+3(21),WORK     SET TO DOTS                                  
         SPACE 2                                                                
OB4      CLI   0(R3),1             1=O (ME)                                     
         BNE   *+8                                                              
         MVI   0(R5),C'O'                                                       
         CLI   0(R3),2             2=X (YOU)                                    
         BNE   *+8                                                              
         MVI   0(R5),C'X'                                                       
         LA    R5,3(R5)                                                         
         LA    R3,1(R3)                                                         
         BCT   R6,OB4                                                           
         CLC   WORK(22),14(R2)                                                  
         BE    OB6                                                              
         MVC   14(22,R2),WORK                                                   
         OI    6(R2),X'80'                                                      
         SPACE 2                                                                
OB6      BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R4,OB2                                                           
         B     XIT                                                              
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              BOARD TABLES                                                     
         SPACE 3                                                                
BOARDADS DS    0F                                                               
         DC    A(PL01),A(PL02),A(PL03),A(PL04)                                  
         DC    A(PL05),A(PL06),A(PL07),A(PL08)                                  
         DC    A(PL09),A(PL10),A(PL11),A(PL12)                                  
         DC    A(PL13),A(PL14),A(PL15),A(PL16)                                  
         DC    A(PL17),A(PL18),A(PL19),A(PL20)                                  
         DC    A(PL21),A(PL22),A(PL23),A(PL24)                                  
         DC    A(PL25),A(PL26),A(PL27),A(PL28)                                  
         DC    A(PL29),A(PL30),A(PL31),A(PL32)                                  
         DC    A(PL33),A(PL34),A(PL35),A(PL36)                                  
         DC    A(PL37),A(PL38),A(PL39),A(PL40)                                  
         DC    A(PL41),A(PL42),A(PL43),A(PL44)                                  
         DC    A(PL45),A(PL46),A(PL47),A(PL48)                                  
         DC    A(PL49),A(PL50),A(PL51),A(PL52)                                  
         DC    A(PL53),A(PL54),A(PL55),A(PL56)                                  
         DC    A(PL57),A(PL58),A(PL59),A(PL60)                                  
         DC    A(PL61),A(PL62),A(PL63),A(PL64)                                  
         SPACE 2                                                                
PL01     DC    AL1(2,3,4,5,6,7,8,0)                                             
         DC    AL1(9,17,25,33,41,49,57,0)                                       
         DC    AL1(10,19,28,37,46,55,64,0,255)                                  
PL02     DC    AL1(3,4,5,6,7,8,0)                                               
         DC    AL1(10,18,26,34,42,50,58,0)                                      
         DC    AL1(11,20,29,38,47,56,0,255)                                     
PL03     DC    AL1(2,1,0,4,5,6,7,8,0)                                           
         DC    AL1(11,19,27,35,43,51,59,0)                                      
         DC    AL1(10,17,0,12,21,30,39,48,0,255)                                
PL04     DC    AL1(3,2,1,0,5,6,7,8,0)                                           
         DC    AL1(12,20,28,36,44,52,60,0)                                      
         DC    AL1(11,18,25,0,13,22,31,40,0,255)                                
PL05     DC    AL1(4,3,2,1,0,6,7,8,0)                                           
         DC    AL1(13,21,29,37,45,53,61,0)                                      
         DC    AL1(12,19,26,33,0,14,23,32,0,255)                                
PL06     DC    AL1(5,4,3,2,1,0,7,8,0)                                           
         DC    AL1(14,22,30,38,46,54,62,0)                                      
         DC    AL1(13,20,27,34,41,0,15,24,0,255)                                
PL07     DC    AL1(6,5,4,3,2,1,0)                                               
         DC    AL1(15,23,31,39,47,55,63,0)                                      
         DC    AL1(14,21,28,35,42,49,0,255)                                     
PL08     DC    AL1(7,6,5,4,3,2,1,0)                                             
         DC    AL1(16,24,32,40,48,56,64,0)                                      
         DC    AL1(15,22,29,36,43,50,57,0,255)                                  
PL09     DC    AL1(10,11,12,13,14,15,16,0)                                      
         DC    AL1(17,25,33,41,49,57,0)                                         
         DC    AL1(18,27,36,45,54,63,0,255)                                     
PL10     DC    AL1(11,12,13,14,15,16,0)                                         
         DC    AL1(18,26,34,42,50,58,0)                                         
         DC    AL1(19,28,37,46,55,64,0,255)                                     
PL11     DC    AL1(10,9,0,12,13,14,15,16,0)                                     
         DC    AL1(19,27,35,43,51,59,0)                                         
         DC    AL1(18,25,0,20,29,38,47,56,0,255)                                
PL12     DC    AL1(11,10,9,0,13,14,15,16,0)                                     
         DC    AL1(20,28,36,44,52,60,0)                                         
         DC    AL1(19,26,33,0,21,30,39,48,0,255)                                
PL13     DC    AL1(12,11,10,9,0,14,15,16,0)                                     
         DC    AL1(21,29,37,45,53,61,0)                                         
         DC    AL1(20,27,34,41,0,22,31,40,0,255)                                
PL14     DC    AL1(13,12,11,10,9,0,15,16,0)                                     
         DC    AL1(22,30,38,46,54,62,0)                                         
         DC    AL1(21,28,35,42,49,0,23,32,0,255)                                
PL15     DC    AL1(14,13,12,11,10,9,0)                                          
         DC    AL1(23,31,39,47,55,63,0)                                         
         DC    AL1(22,29,36,43,50,57,0,255)                                     
PL16     DC    AL1(15,14,13,12,11,10,9,0)                                       
         DC    AL1(24,32,40,48,56,64,0)                                         
         DC    AL1(23,30,37,44,51,58,0,255)                                     
PL17     DC    AL1(18,19,20,21,22,23,24,0)                                      
         DC    AL1(9,1,0,25,33,41,49,57,0)                                      
         DC    AL1(10,3,0,26,35,44,53,62,0,255)                                 
PL18     DC    AL1(19,20,21,22,23,24,0)                                         
         DC    AL1(10,2,0,26,34,42,50,58,0)                                     
         DC    AL1(11,4,0,27,36,45,54,63,0,255)                                 
PL19     DC    AL1(18,17,0,20,21,22,23,24,0)                                    
         DC    AL1(11,3,0,27,35,43,51,59,0)                                     
         DC    AL1(10,1,0,28,37,46,55,64,0,12,5,0,26,33,0,255)                  
PL20     DC    AL1(19,18,17,0,21,22,23,24,0)                                    
         DC    AL1(12,4,0,28,36,44,52,60,0)                                     
         DC    AL1(11,2,0,29,38,47,56,0,13,6,0,27,34,41,0,255)                  
PL21     DC    AL1(20,19,18,17,0,22,23,24,0)                                    
         DC    AL1(13,5,0,29,37,45,53,61,0)                                     
         DC    AL1(12,3,0,30,39,48,0,14,7,0,28,35,42,49,0,255)                  
PL22     DC    AL1(21,20,19,18,17,0,23,24,0)                                    
         DC    AL1(14,6,0,30,38,46,54,62,0)                                     
         DC    AL1(13,4,0,31,40,0,15,8,0,29,36,43,50,57,0,255)                  
PL23     DC    AL1(22,21,20,19,18,17,0)                                         
         DC    AL1(15,7,0,31,39,47,55,63,0)                                     
         DC    AL1(14,5,0,30,37,44,51,58,0,255)                                 
PL24     DC    AL1(23,22,21,20,19,18,17,0)                                      
         DC    AL1(16,8,0,32,40,48,56,64,0)                                     
         DC    AL1(15,6,0,31,38,45,52,59,0,255)                                 
PL25     DC    AL1(26,27,28,29,30,31,32,0)                                      
         DC    AL1(17,9,1,0,33,41,49,57,0)                                      
         DC    AL1(34,43,52,61,0,18,11,4,0,255)                                 
PL26     DC    AL1(27,28,29,30,31,32,0)                                         
         DC    AL1(18,10,2,0,34,42,50,58,0)                                     
         DC    AL1(35,44,53,62,0,19,12,5,0,255)                                 
PL27     DC    AL1(26,25,0,28,29,30,31,32,0)                                    
         DC    AL1(19,11,3,0,35,43,51,59,0)                                     
         DC    AL1(18,9,0,36,45,54,63,0,20,13,6,0,27,34,41,0,255)               
PL28     DC    AL1(27,26,25,0,29,30,31,32,0)                                    
         DC    AL1(20,12,4,0,36,44,52,60,0)                                     
         DC    AL1(19,10,1,0,37,46,55,64,0,21,14,7,35,42,49,0,255)              
PL29     DC    AL1(28,27,26,25,0,30,31,32,0)                                    
         DC    AL1(21,13,5,0,37,45,53,61,0)                                     
         DC    AL1(20,11,2,0,38,47,56,0,22,15,8,0,36,43,50,57,0,255)            
PL30     DC    AL1(29,28,27,26,25,0,31,32,0)                                    
         DC    AL1(22,14,6,0,38,46,54,62,0)                                     
         DC    AL1(21,12,3,0,39,48,0,23,16,0,37,44,51,58,0,255)                 
PL31     DC    AL1(30,29,28,27,26,25,0)                                         
         DC    AL1(23,15,7,0,39,47,55,63,0)                                     
         DC    AL1(22,13,4,0,38,45,52,59,0,255)                                 
PL32     DC    AL1(31,30,29,28,27,26,25,0)                                      
         DC    AL1(24,16,8,0,40,48,56,64,0)                                     
         DC    AL1(23,14,5,0,39,46,53,60,0,255)                                 
PL33     DC    AL1(34,35,36,37,38,39,40,0)                                      
         DC    AL1(25,17,9,1,0,41,49,59,0)                                      
         DC    AL1(42,51,60,0,26,19,12,5,0,255)                                 
PL34     DC    AL1(35,36,37,38,39,40,0)                                         
         DC    AL1(26,18,10,2,0,42,50,58,0)                                     
         DC    AL1(43,52,61,0,27,20,13,6,0,255)                                 
PL35     DC    AL1(34,33,0,36,37,38,39,40,0)                                    
         DC    AL1(27,19,11,3,0,43,51,59,0)                                     
         DC    AL1(26,17,0,44,53,62,0,28,21,14,7,0,42,49,0,255)                 
PL36     DC    AL1(35,34,33,0,37,38,39,40,0)                                    
         DC    AL1(28,20,12,4,0,44,52,60,0)                                     
         DC    AL1(27,18,9,0,45,54,63,0,29,22,15,8,0,43,50,57,0,255)            
PL37     DC    AL1(36,35,34,33,0,38,39,40,0)                                    
         DC    AL1(29,21,13,5,0,45,53,61,0)                                     
         DC    AL1(28,19,10,1,0,46,55,64,0,30,23,16,0,44,51,58,0,255)           
PL38     DC    AL1(37,36,35,34,33,0,39,40,0)                                    
         DC    AL1(30,22,14,6,0,46,54,62,0)                                     
         DC    AL1(29,20,11,2,0,47,56,0,31,24,0,45,52,59,0,255)                 
PL39     DC    AL1(38,37,36,35,34,33,0)                                         
         DC    AL1(31,23,15,7,0,47,55,63,0)                                     
         DC    AL1(30,21,12,3,0,46,53,60,0,255)                                 
PL40     DC    AL1(39,38,37,36,35,34,33,0)                                      
         DC    AL1(32,24,16,8,0,48,56,64,0)                                     
         DC    AL1(31,22,13,4,0,47,54,61,0,255)                                 
PL41     DC    AL1(42,43,44,45,46,47,48,0)                                      
         DC    AL1(33,25,17,9,1,0,49,57,0)                                      
         DC    AL1(34,27,20,13,6,0,50,59,0,255)                                 
PL42     DC    AL1(43,44,45,46,47,48,0)                                         
         DC    AL1(34,26,18,10,2,0,50,58,0)                                     
         DC    AL1(51,60,0,35,28,21,14,7,0,255)                                 
PL43     DC    AL1(42,41,0,44,45,46,47,48,0)                                    
         DC    AL1(35,27,19,11,3,0,51,59,0)                                     
         DC    AL1(34,25,0,52,61,0,36,29,22,15,8,0,50,57,0,255)                 
PL44     DC    AL1(43,42,41,0,45,46,47,48,0)                                    
         DC    AL1(36,28,20,12,4,0,52,60,0)                                     
         DC    AL1(35,26,17,0,53,62,0,37,30,23,16,0,51,58,0,255)                
PL45     DC    AL1(44,43,42,41,0,46,47,48,0)                                    
         DC    AL1(37,29,21,13,5,0,53,61,0)                                     
         DC    AL1(36,27,18,9,0,54,63,0,38,31,24,0,52,59,0,255)                 
PL46     DC    AL1(45,44,43,42,41,0,47,48,0)                                    
         DC    AL1(38,30,22,14,6,0,54,62,0)                                     
         DC    AL1(37,28,19,10,1,0,55,64,0,39,32,0,53,60,0,255)                 
PL47     DC    AL1(46,45,44,43,42,41,0)                                         
         DC    AL1(39,31,23,15,7,0,55,63,0)                                     
         DC    AL1(38,29,20,11,2,0,54,61,0,255)                                 
PL48     DC    AL1(47,46,45,44,43,42,41,0)                                      
         DC    AL1(40,32,24,16,8,0,56,64,0)                                     
         DC    AL1(39,30,21,12,3,0,55,62,0,255)                                 
PL49     DC    AL1(50,51,52,53,54,55,56,0)                                      
         DC    AL1(41,33,25,17,9,1,0)                                           
         DC    AL1(42,35,28,21,14,7,0,255)                                      
PL50     DC    AL1(51,52,53,54,55,56,0)                                         
         DC    AL1(42,34,26,18,10,2,0)                                          
         DC    AL1(43,36,29,22,15,8,0,255)                                      
PL51     DC    AL1(50,49,0,52,53,54,55,56,0)                                    
         DC    AL1(43,35,27,19,11,3,0)                                          
         DC    AL1(42,33,0,44,37,30,23,16,0,255)                                
PL52     DC    AL1(51,50,49,0,53,54,55,56,0)                                    
         DC    AL1(44,36,28,20,12,4,0)                                          
         DC    AL1(43,34,25,0,45,38,31,24,0,255)                                
PL53     DC    AL1(52,51,50,49,0,54,55,56,0)                                    
         DC    AL1(45,37,29,21,13,5,0)                                          
         DC    AL1(44,35,26,17,0,46,39,32,0,255)                                
PL54     DC    AL1(53,52,51,50,49,0,55,56,0)                                    
         DC    AL1(46,38,30,22,14,6,0)                                          
         DC    AL1(45,36,27,18,9,0,47,40,0,255)                                 
PL55     DC    AL1(54,53,52,51,50,49,0)                                         
         DC    AL1(47,39,31,23,15,7,0)                                          
         DC    AL1(46,37,28,19,10,1,0,255)                                      
PL56     DC    AL1(55,54,53,52,51,50,49,0)                                      
         DC    AL1(48,40,32,24,16,8,0)                                          
         DC    AL1(47,38,29,20,11,2,0,255)                                      
PL57     DC    AL1(58,59,60,61,62,63,64,0)                                      
         DC    AL1(49,41,33,25,17,9,1,0)                                        
         DC    AL1(50,43,36,29,22,15,8,0,255)                                   
PL58     DC    AL1(59,60,61,62,63,64,0)                                         
         DC    AL1(50,42,34,26,18,10,2,0)                                       
         DC    AL1(51,44,37,30,23,16,0,255)                                     
PL59     DC    AL1(58,57,0,60,61,62,63,64,0)                                    
         DC    AL1(51,43,35,27,19,11,3,0)                                       
         DC    AL1(50,41,0,52,45,38,31,24,0,255)                                
PL60     DC    AL1(59,58,57,0,61,62,63,64,0)                                    
         DC    AL1(52,44,36,28,20,12,4,0)                                       
         DC    AL1(51,42,33,0,53,46,39,32,0,255)                                
PL61     DC    AL1(60,59,58,57,0,62,63,64,0)                                    
         DC    AL1(53,45,37,29,21,13,5,0)                                       
         DC    AL1(52,43,34,25,0,54,47,40,0,255)                                
PL62     DC    AL1(61,60,59,58,57,0,63,64,0)                                    
         DC    AL1(54,46,38,30,22,14,6,0)                                       
         DC    AL1(53,44,35,26,17,0,55,48,0,255)                                
PL63     DC    AL1(62,61,60,59,58,57,0)                                         
         DC    AL1(55,47,39,31,23,15,7,0)                                       
         DC    AL1(54,45,36,27,18,9,0,255)                                      
PL64     DC    AL1(63,62,61,60,59,58,57,0)                                      
         DC    AL1(56,48,40,32,24,16,8,0)                                       
         DC    AL1(55,46,37,28,19,10,1,0,255)                                   
         SPACE 2                                                                
VALUETAB DC    AL1(30,10,12,10,10,12,10,30)                                     
         DC    AL1(10,00,00,00,00,00,00,10)                                     
         DC    AL1(12,00,05,03,03,05,00,12)                                     
         DC    AL1(10,00,03,00,00,03,00,10)                                     
         DC    AL1(10,00,03,00,00,03,00,10)                                     
         DC    AL1(12,00,05,03,03,05,00,12)                                     
         DC    AL1(10,00,00,00,00,00,00,10)                                     
         DC    AL1(30,10,12,10,10,12,10,30)                                     
         SPACE 2                                                                
ORIGINAL DC    27X'00',AL1(1,2),6X'00',AL1(2,1),27X'00'                         
         SPACE 2                                                                
BOARDLET DC    C'A1B1C1D1E1F1G1H1'                                              
         DC    C'A2B2C2D2E2F2G2H2'                                              
         DC    C'A3B3C3D3E3F3G3H3'                                              
         DC    C'A4B4C4D4E4F4G4H4'                                              
         DC    C'A5B5C5D5E5F5G5H5'                                              
         DC    C'A6B6C6D6E6F6G6H6'                                              
         DC    C'A7B7C7D7E7F7G7H7'                                              
         DC    C'A8B8C8D8E8F8G8H8'                                              
         SPACE 2                                                                
SPACES   DC    CL80' '                                                          
         SPACE 2                                                                
MESSA    DC    C'I MOVED TO XX.  YOU NOW HAVE NN PIECES, I HAVE MM '            
MESSB    DC    C'CONGRATULATIONS.  YOU WON THE GAME NN PIECES TO MM'            
MESSC    DC    C'I WON THE GAME MM PIECES TO NN.  THANK YOU.       '            
MESSD    DC    C'YOUR MOVE IS INVALID - NOT A RECOGNISED MOVE      '            
MESSE    DC    C'YOUR MOVE IS INVALID - NO PIECES FLIPPED          '            
MESSF    DC    C'YOUR MOVE IS INVALID - A PIECE IS ALREADY THERE   '            
MESSG    DC    C'WE TIED WITH 32 PIECES EACH. THANK YOU.           '            
         EJECT                                                                  
       ++INCLUDE GAOTHFFD                                                       
DUB      DS    D                                                                
WORK     DS    CL32                                                             
BOARD    DS    CL64                                                             
MYMEN    DS    F                                                                
YOURMEN  DS    F                                                                
FIRSTIME DS    CL1                                                              
SCORE    DS    F                                                                
BEST     DS    F                                                                
SAVBOARD DS    CL64                                                             
MYMOVE   DS    CL2                                                              
MOVENO   DS    CL1                                                              
BESTB    DS    CL64                                                             
SAVEMOVE DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GAOTH00   05/01/02'                                      
         END                                                                    

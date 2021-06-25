*          DATA SET DDQSORTS   AT LEVEL 037 AS OF 05/01/02                      
*PHASE T00A12A                                                                  
         TITLE 'QSORT - QUICK ITERATIVE SORT MODULE'                            
         SPACE 3                                                                
*        PARAMETER LIST                                                         
         SPACE 1                                                                
*        PARAM1  BYTE  0      X'00' = ASCENDING,X'FF' = DESCENDING              
*                BYTES 1-3    A(LIST)                                           
         SPACE 1                                                                
*        PARAM2  BYTES 0-3    NO. OF ENTRIES                                    
         SPACE 1                                                                
*        PARAM3  BYTE    3    LENGTH OF ENTRY                                   
         SPACE 1                                                                
*        PARAM4  BYTE    3    LENGTH OF KEY                                     
         SPACE 1                                                                
*        PARAM5  BYTE    3    DISPLACEMENT OF KEY INTO ENTRY                    
         SPACE 1                                                                
         PRINT NOGEN                                                            
QSORT    CSECT                                                                  
         NMOD1 WORKX-WORKD,**QSRT**                                             
         USING WORKD,RC            RC=A(W/S)                                    
         MVC   PARMTAB,0(R1)                                                    
         EJECT                                                                  
*             PROCESS PARAMETER LIST                                            
*                                                                               
PROC     CLC   NUMELEMS,=F'1'                                                   
         BNH   EXIT                                                             
*                                                                               
         MVC   WORKINS,CHARINS     MOVE CHARACTER INSTRUCTIONS TO WORK          
         CLI   PARMTAB,X'00'       IF DESCENDING SORT                           
         BE    PROC1                                                            
         MVC   CTJV(6),DESCEND     MOVE IN SPECIAL DESCENDING COMPARES          
*                                                                               
PROC1    L     R1,KEYLEN           PREPARE CLC INSTRUCTIONS                     
         BCTR  R1,0                                                             
         STC   R1,CTJV+1                                                        
         STC   R1,CTIV+1                                                        
         L     R2,KEYDISP                                                       
         STC   R2,CTJV+3                                                        
         STC   R2,CTIV+3                                                        
         STC   R2,CTJV+5                                                        
         STC   R2,CTIV+5                                                        
         STC   R2,BUBCLC+3                                                      
         STC   R2,BUBCLC+5                                                      
*                                                                               
         L     R1,ELEMLEN          PREPARE MOVE INSTRUCTIONS                    
         BCTR  R1,0                                                             
         STC   R1,VEQTP+1                                                       
         STC   R1,TPEQTJ+1                                                      
         STC   R1,TJEQV+1                                                       
*                                                                               
         STC   R1,PARTXC1+1        PREPARE XC INSTRUCTIONS                      
         STC   R1,PARTXC2+1                                                     
*                                                                               
         SR    R9,R9               ISORT(0,NUMELEMS-1)                          
         L     RA,NUMELEMS                                                      
         BCTR  RA,0                                                             
         BAS   RE,ISORT                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
*                      ISORT(LOW,HIGH)                                          
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = SCRATCH PAD REGISTER                                              
*        R3 = SCRATCH PAD REGISTER                                              
*        R4 = NOT USED                                                          
*        R5 = NOT USED                                                          
*        R6 = P                    LOCAL VARIABLES                              
*        R7 = Q                                                                 
*        R8 = STACK POINTER                                                     
*        R9 = LOW   /  P           FUNCTION ARGUMENTS                           
*        RA = HIGH  /  J                                                        
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
ISORT    NTR1                                                                   
         LR    R6,R9               MOVE ARGS LOW,HIGH TO LOCAL P,Q              
         LR    R7,RA                                                            
         LA    R8,STACK                                                         
ISORT1   CR    R6,R7               WHILE P ( Q                                  
         BNL   ISORT10                                                          
         LA    RA,1(R7)            J = Q + 1                                    
         LR    R9,R6               PARTITION(P,J)                               
         BAS   RE,PART                                                          
         LR    R1,RA               IF J - P ( Q - J                             
         SR    R1,R6                                                            
         LR    R2,R7                                                            
         SR    R2,RA                                                            
         CR    R1,R2                                                            
         BNL   ISORT2                                                           
         LA    R3,1(RA)            THEN STACK(TOP) = J + 1                      
         ST    R3,0(R8)                                                         
         ST    R7,4(R8)            STACK(TOP+1) = Q                             
         LR    R7,RA               Q = J - 1                                    
         BCTR  R7,0                                                             
         B     ISORT3                                                           
ISORT2   ST    R6,0(R8)            ELSE STACK(TOP) = P                          
         LR    R3,RA               STACK(TOP+1) = J - 1                         
         BCTR  R3,0                                                             
         ST    R3,4(R8)                                                         
         LA    R6,1(RA)                                                         
ISORT3   LA    R8,8(R8)            BUMP STACK TO STACK(TOP+2)                   
         B     ISORT1                                                           
ISORT10  LA    R1,STACK            IF STACK EMPTY                               
         CR    R8,R1                                                            
         BE    EXIT                THEN EXIT                                    
         S     R8,=F'8'            BACK UP STACK TO STACK(TOP-2)                
         L     R6,0(R8)            P = STACK(TOP)                               
         L     R7,4(R8)            Q = STACK(TOP+1)                             
         B     ISORT1                                                           
         EJECT                                                                  
*                      PARTITION(P,J)                                           
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = NOT USED                                                          
*        R3 = NOT USED                                                          
*        R4 = NOT USED                                                          
*        R5 = I                    LOCAL VARIABLES                              
*        R6 = TABLE(P)                                                          
*        R7 = TABLE(I)                                                          
*        R8 = TABLE(J)                                                          
*        R9 = P                    FUNCTION ARGUMENTS                           
*        RA = J                                                                 
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
PART     NTR1                                                                   
         L     R6,ATABLE           POINT R6 TO TABLE(P)                         
         LR    R1,R9                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R6,R1                                                            
         LR    R5,R9               I = P                                        
         LR    R7,R6               POINT R7 TO TABLE(I)                         
         L     R8,ATABLE           POINT R8 TO TABLE(J)                         
         LR    R1,RA                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R8,R1                                                            
         EX    R0,VEQTP            V = TABLE(P)                                 
PART1    BCTR  RA,0                J = J - 1                                    
         S     R8,ELEMLEN          BACK UP TABLE(J)                             
         EX    R0,CTJV             REPEAT UNTIL TABLE(J) (= V                   
         BH    PART1                                                            
PART2    LA    R5,1(R5)            I = I + 1                                    
         A     R7,ELEMLEN          BUMP TABLE(I)                                
         CR    R5,RA               REPEAT UNTIL I )= J                          
         BNL   PART3                                                            
         EX    R0,CTIV             OR TABLE(I) ) V                              
         BNH   PART2                                                            
PART3    CR    R5,RA               IF I ( J                                     
         BNL   PART4                                                            
         EX    R0,PARTXC1          SWITCH TABLE(I),TABLE(J)                     
         EX    R0,PARTXC2                                                       
         EX    R0,PARTXC1                                                       
         B     PART1               REPEAT LOOP                                  
PART4    EX    R0,TPEQTJ           ELSE TABLE(P) = TABLE(J)                     
         EX    R0,TJEQV            TABLE(J) = V                                 
         DC    H'0'                                                             
         XIT1  REGS=(RA)           RETURN J                                     
         EJECT                                                                  
*                      BUBBLE(LOW,HIGH)                                         
*                                                                               
*        R0 = NOT USED                                                          
*        R1 = SCRATCH PAD REGISTER                                              
*        R2 = NOT USED                                                          
*        R3 = I                    LOCAL VARIABLES                              
*        R4 = J                                                                 
*        R5 = BACKUP()                                                          
*        R6 = TABLE(I)                                                          
*        R7 = TABLE(J)                                                          
*        R8 = LOW                  FUNCTION ARGUMENTS                           
*        R9 = MID                                                               
*        RA = HIGH                                                              
*        RB = BASE REGISTER                                                     
*        RC = WORK AREA ADDRESS                                                 
*        RD = REGISTER SAVE CHAIN                                               
*        RE = RETURN ADDRESS                                                    
*        RF = NOT USED                                                          
*                                                                               
BUBBLE   NTR1                                                                   
         LR    R3,R8               I = LOW                                      
         L     R6,ATABLE           POINT R6 TO TABLE(I)                         
         LR    R1,R3                                                            
         MH    R1,ELEMLEN+2                                                     
         AR    R6,R1                                                            
*                                                                               
BUBBLE1  CR    R3,RA               WHILE I ( HIGH                               
         BNL   EXIT                                                             
         LA    R4,1(R3)            J = I + 1                                    
         LR    R7,R6               POINT R7 TO TABLE(J)                         
         A     R7,ELEMLEN                                                       
*                                                                               
BUBBLE2  CR    R4,RA               WHILE J (= HIGH                              
         BH    BUBBLE4                                                          
*                                                                               
BUBCLC   CLC   0(0,R6),0(R7)       IF TABLE(I),TABLE(J) NOT IN ORDER            
BUBBC    BC    0,BUBBLE3                                                        
BUBXC1   XC    0(0,R6),0(R7)       THEN SWITCH THEM                             
BUBXC2   XC    0(0,R7),0(R6)                                                    
BUBXC3   XC    0(0,R6),0(R7)                                                    
*                                                                               
BUBBLE3  LA    R4,1(R4)            J = J + 1                                    
         A     R7,ELEMLEN          BUMP TABLE(J)                                
         B     BUBBLE2                                                          
*                                                                               
BUBBLE4  LA    R3,1(R3)            I = I + 1                                    
         A     R6,ELEMLEN          BUMP TABLE(I)                                
         B     BUBBLE1                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        SPECIAL CHARACTER INSTRUCTIONS                                         
*                                                                               
CHARINS  MVC   V(0),0(R6)          V = TABLE(P)                                 
         CLC   0(0,R8),V           COMPARE TABLE(J) TO V                        
         CLC   0(0,R7),V           COMPARE TABLE(I) TO V                        
         XC    0(0,R7),0(R8)       SWITCH TABLE(I),TABLE(J)                     
         XC    0(0,R8),0(R7)                                                    
         MVC   0(0,R6),0(R8)       TABLE(P) = TABLE(J)                          
         MVC   0(0,R8),V           TABLE(J) = V                                 
*                                                                               
DESCEND  CLC   V(0),0(R8)          COMPARES FOR DESCENDING SORT                 
         CLC   V(0),0(R7)                                                       
         EJECT                                                                  
* WORKING STORAGE AREA                                                          
*                                                                               
WORKD    DSECT                                                                  
*                                  V MUST EQUAL ZERO PAST REGISTER C            
V        DS    256C                V IS THE PARTITION ELEMENT                   
*                                                                               
PARMTAB  DS    0CL24                                                            
ATABLE   DS    F                                                                
NUMELEMS DS    F                                                                
ELEMLEN  DS    F                                                                
KEYLEN   DS    F                                                                
KEYDISP  DS    F                                                                
ABACKUP  DS    F                                                                
*                                                                               
STACK    DS    64F                                                              
*                                                                               
WORKINS  DS    0CL42                                                            
VEQTP    DS    3H                  V = TABLE(P)                                 
CTJV     DS    3H                  COMPARE TABLE(J) TO V                        
CTIV     DS    3H                  COMPARE TABLE(I) TO V                        
PARTXC1  DS    3H                  SWITCH TABLE(I),TABLE(J)                     
PARTXC2  DS    3H                                                               
TPEQTJ   DS    3H                  TABLE(P) = TABLE(J)                          
TJEQV    DS    3H                  TABLE(J) = V                                 
*                                                                               
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037DDQSORTS  05/01/02'                                      
         END                                                                    
